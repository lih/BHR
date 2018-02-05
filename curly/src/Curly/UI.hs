{-# LANGUAGE ViewPatterns,TypeFamilies #-}
module Curly.UI(
  -- * Variables
  curlyPort,curlyUserDir,curlyHistoryFile,

  -- * Arguments
  CurlyConfig,
  parseCurlyArgs,withCurlyPlex,withCurlyConfig,
  curlyFiles,
  
  -- * Contexts
  withMountain,reloadMountain,sourceFile,

  -- * Misc
  watchSources,sourceLibs,builtinLibs,getVCSBranches,curlyDataFileName,getDataFileName_ref
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq (deepseq)
import Control.DeepSeq (force)
import Control.Exception hiding (throw)
import Curly.Core
import Curly.Core.Annotated
import Curly.Core.Documentation
import Curly.Core.Library
import Curly.Core.Parser
import Curly.Core.Security
import Curly.Core.Security.SHA256 (hashlazy)
import Curly.Core.VCS
import Curly.UI.Options
import Data.IORef 
import Data.List (sortBy)
import IO.Filesystem hiding ((</>))
import IO.Time
import Language.Format
import Language.Syntax.CmdArgs hiding (hspace)
import System.IO (IOMode(..),withFile)
import System.Posix.Files (createSymbolicLink,removeLink,fileAccess)
import System.Environment (getExecutablePath)

withMountain :: (?curlyPlex :: CurlyPlex,MonadIO m) => ((?mountain :: Mountain) => m a) -> m a
withMountain m = liftIO (trylogLevel Quiet (return undefined) $ readIORef (?curlyPlex^.mountainCache)) >>= \(c,_) -> let ?mountain = c in m
reloadMountain :: (?curlyPlex :: CurlyPlex,MonadIO m) => m ()
reloadMountain = liftIOLog $ do
  m <- mountain
  callbacks <- runAtomic (?curlyPlex^.mountainCache) $ do
    l'1 =- m
    getl l'2
  liftIO $ traverse_ ($m) callbacks

makeFileModule :: (FileAttrs -> Bytes -> Maybe String -> (String,FileLibrary))
                  -> (String -> String)
                  -> File -> Module FileLibrary
makeFileModule getLib transformFileName x =
  let inc (File a s (Just bs)) = Pure $ warp flLibrary rename lib
        where (n',lib) = getLib a bs s
              rename = warp exports f
                where f (Pure (GlobalID _ l,v)) = Pure (GlobalID n' l,v)
                      f y = y
      inc (Directory m) = Join (ModDir (m^.ascList & map snd . sortBy (comparing fst) . \l -> l <&> \(s,e) ->
                                          let (n,s') = curlyFileName s in (n,(s',inc e))))
      inc _ = zero
      modDir (Directory m) = Directory (m&ascList %~ \l -> [(case f of
                                                                File _ _ _ -> transformFileName s
                                                                _ -> s,
                                                             modDir f)
                                                           | (s,f) <- l])
      modDir (File a b t) = File (a&relPath %~ transformFileName) b t
  in inc (modDir x)

sourceFile :: (?mountain :: Mountain) => [String] -> (String,String) -> File -> Module FileLibrary
sourceFile base dirs =
  let ?mountain = fromMaybe zero (?mountain^?atMs base)
  in makeFileModule
     (\a _ s -> (snd (curlyFileName (takeFileName (a^.relPath))),
                 cacheCurly dirs a s))
     (\x -> fromMaybe x (noCurlySuf x))

resourceFile :: (String,String) -> File -> Module FileLibrary
resourceFile dirs = makeFileModule
                    (\a bs _ -> (takeFileName (a^.relPath)
                                ,cacheResource dirs a bs))
                    id

cacheResource :: (String,String) -> FileAttrs -> Bytes -> FileLibrary
cacheResource (src,cache) a bs = by thunk $ do
  isInvalid <- (>) (a^.lastMod) <$> modTime cacheName
  if isInvalid
    then do
    let canPath = cacheFileName curlyCacheDir (show (lib^.flID)) "cyl"
    createFileDirectory cacheName ; createFileDirectory canPath
    writeBytes canPath (lib^.flBytes)
    modifyPermissions canPath (set (each.executePerm) True)
    trylog unit $ removeLink cacheName
    createSymbolicLink canPath cacheName
    return lib
    else do
    ser <- slurpBytes cacheName
    return
      $ maybe (error $ format "%s: Invalid library file format" cacheName) (\l -> rawLibrary False l ser Nothing)
      $ matches Just datum ser
  where bval = B_Bytes bs
        sym = mkSymbol (pureIdent "value",Pure (Builtin (builtinType bval) bval))
        lib = fileLibrary (zero
                           & set symbols (singleton "value" (undefLeaf ("Data resource: "+(a^.relPath))
                                                             & set leafVal sym
                                                             & set leafType (builtinType bval)
                                                             & set leafPos (SourceRange (Just sourceName) (0,0,0) (0,0,0))))
                           & setExports (Pure "value")) (Just sourceName)
  
        cacheName = cache+a^.relPath+".cyl"
        sourceName = src+a^.relPath
        
        
                    
cacheCurly :: (?mountain :: Mountain) => (String,String) -> FileAttrs -> Maybe String -> FileLibrary
cacheCurly (src,cache) a ms = by thunk $ do
  let filename d e = case a^.relPath of
        "" -> d
        p -> d+p+"."+e
      cacheName = filename cache "cyl"
      sourceName = filename src "cy"
      addSource = warp (flLibrary.((symbols.traverse).+((imports.+exports).traverse.l'2)).leafPos) setR
        where setR (SourceRange f a b) = SourceRange (f + Just sourceName) a b
              setR NoRange = NoRange
      readSourceFile = case ms of
        Just s -> case getId (parseCurly (force s) (curlyFile <* eoi)) of
          Right f' -> do
            keyInfo <- getKeyStore <&> \ks x -> lookup x ks <&> \(_,pub,_,Metadata meta,_) -> (pub,meta)
            time <- currentTime
            let f = case envVar "" "CURLY_PUBLISHER" of
                  "" -> f'
                  x -> f' & metadata.iso (\(Metadata m) -> m) Metadata
                       %~ insert "publisher" (maybe id (\x -> insert ["public-key"] (Pure (show (Zesty x)))) (fst <$> keyInfo x)
                                              $ withDate
                                              $ Join (maybe zero snd (keyInfo x)))
                       . insert "context" (mapF (\(ModDir d) -> fromAList d)
                                           $ shortZipWith (const . show . by flID) ?mountain (f'^.imports))
                withDate x | x^?at ["timestamp"].t'Just.t'Pure == Just "date" = insert ["timestamp"] (Pure (show (floor (1000*time)))) x
                           | otherwise = x
                shortZipWith f a x = case x of
                  Join m' | nonempty m' -> case a of
                    Join m -> Join (zipWith (shortZipWith f) m m')
                    Pure a' -> Pure (f a' x)
                          | otherwise -> Join zero
                  _ -> map (`f`x) a
                ser = serialize f
                lid = LibraryID (hashlazy ser)
                canPath = cacheFileName curlyCacheDir (show lid) "cyl"
            traverse_ createFileDirectory [canPath,cacheName]
            trylog unit $ do
              writeBytes canPath ser
              modifyPermissions canPath (set (each.executePerm) True)
            trylog unit $ removeLink cacheName
            createSymbolicLink canPath cacheName
            return (rawLibrary True f ser ms)
          Left ws -> return $ throw (toException $ CurlyParserException (Just sourceName) ws)^.thunk
        Nothing -> error $ sourceName+" doesn't seem to be a textual file"
              
  b <- (>) (a^.lastMod) <$> modTime cacheName

  addSource <$>
    if b
    then readSourceFile
    else do 
      ser <- slurpBytes cacheName
      case matches Just datum ser of
        Just l | or (zipWith (\_ fl -> fl^.flFromSource) (l^.imports) ?mountain) -> do
                   logLine Verbose $ format "Reloading source file %s" sourceName
                   readSourceFile
               | otherwise -> return (rawLibrary False l ser ms)
        _ -> error $ format "%s: Invalid library file format" cacheName

slurpBytes :: String -> IO Bytes
slurpBytes x = yb chunk <$> withFile x ReadMode (\h -> readHChunk h <*= \c -> c`deepseq`return ())

mountain :: (?curlyPlex :: CurlyPlex) => IO Mountain
mountain = mfix $ \c -> let ?mountain = c in do
  -- let ren n = t'Pure.flLibrary.exports.t'Pure.l'1 %- pureIdent n
  let ren _ m = m
  mnts <- for (?curlyPlex^.mounts) $ \(p,src) -> do
    mod <- case src of
      Library l -> return $ Pure (fromMaybe (error $ "Could not find library "+show l) (findLib l))
      LibraryFile l -> do
        ser <- slurpBytes l
        let lib = fromMaybe (error $ format "Couldn't parse library file '%s'" l)
                  $ matches Just datum ser
        return $ Pure $ rawLibrary False lib ser Nothing
      Source b s c -> getFile s <&> \f -> sourceFile b (s,c) f
      Resource s c -> resourceFile (s,c) <$> getFile s
    return (atMs p %- ren (last p) mod)
  return $ compose mnts (Join zero)

watchSources :: (?curlyPlex :: CurlyPlex) => IO ()
watchSources = do
  sequence_ [watchFile s reloadMountain | (_,Source _ s _) <- ?curlyPlex^.mounts]
  sequence_ [watchFile f reloadMountain | (_,LibraryFile f) <- ?curlyPlex^.mounts]
  sequence_ [watchFile f reloadMountain | (_,Resource f _) <- ?curlyPlex^.mounts]

parseCurlyArgs :: [String] -> [String :+: [CurlyOpt]]
parseCurlyArgs args = fromMaybe [] $ matches Just (tokenize (map2 Right curlyOpts) naked) args
  where naked ('%':s) = Right [Target (Execute s)]
        naked ('+':s) = Right [Flag s]
        naked ('@':s) = Right [Target (SetServer (readServer s))]
        naked (':':s) = Right [Target (SetInstance s)]
        naked s = case matches Just url s of
          Just t -> Right t
          _ -> Left s
        url = do
          proto <- many1' (noneOf ":")
          path <- single ':' >> remaining
          let lid = packageID (docTag' "=" [docTag' "$" [Pure "name"],Pure path])^.thunk
          (pure proto >*> (like "package" >> eoi)) >> return [Mount [path] (Library lid)]
            

type CurlyConfig = [(Maybe String,CurlyOpt)]

followSymlinks :: String -> IO String
followSymlinks f = return f`trylog`(followSymlinks =<< followSymlink f)

executableDir :: FilePath
executableDir = (map dropFileName . followSymlinks =<< getExecutablePath)^.thunk

getDataFileName_ref :: MVar (String -> IO String)
getDataFileName_ref = newEmptyMVar^.thunk

curlyDataFileName :: String -> IO String
curlyDataFileName n = withMVar getDataFileName_ref $ \gdfn -> do
  ret1 <- gdfn n
  foldr firstEx (return ret1) [ret1,executableDir+n,executableDir+"../share/curly/"+n,executableDir+"../share/"+n]
  where firstEx f k = do
          canRead <- trylog (return False) (fileAccess f True False False)
          if canRead then return f
            else k

i'isJust :: Monoid m => Iso' (Maybe m) Bool
i'isJust = iso (maybe False (const True)) (\b -> if b then Just zero else Nothing)

withCurlyConfig :: [String :+: [CurlyOpt]] -> ((?curlyConfig :: CurlyConfig) => IO a) -> IO a
withCurlyConfig a x = do
  c <- readCurlyConfig a
  let ?curlyConfig = c in x
readCurlyConfig :: [String :+: [CurlyOpt]] -> IO CurlyConfig
readCurlyConfig cliargs = fold <$> traverse (fileArgs [] <|> return . map (Nothing,)) cliargs
  where dropHeadDot ('.':'/':t) = dropHeadDot t
        dropHeadDot x = x
        cliFiles = c'set $ fromKList (map dropHeadDot (cliargs^??each.t'1))
        fileArgs mnt (dropHeadDot -> file) = do
          config <- readString file
          case matches Just (sourceFile <+? objFile) config of
            Just cfg -> return cfg
            Nothing -> do
              file' <- followSymlinks file
              fromMaybe [] <$> matchesT Just (configFile file') config
          where sourceFile = (several "module"+several "symbol") <&> \_ ->
                  [(Nothing,Mount [bareName file] (Source [] file (file+"l")))]
                objFile = several "#!/lib/cyl!#" <&> \_ ->
                  [(Nothing,Mount [bareName file] (LibraryFile file))]
                bareName s = takeFileName s & \x -> fromMaybe x (noCurlySuf x)

                delDefault | file`isKeyIn`cliFiles = fromKList <#> fromKList
                           | otherwise = warp (at "command".i'isJust) not . fromKList <#> fromKList
                configFile s = fold <$> sepBy' (localOpt condDesc <+? condClause) (skipMany' (nbhspace+eol))
                  where clause = localOpt (foldl1' (<+?) [cmd n arg | Option _ ns arg _ <- curlyOpts, n <- ns])
                                 <+? include
                                 <+? localOpt echo
                                 <+? localOpt exe
                                 <+? localOpt flag
                                 <+? localOpt longPrelude
                                 <+? [] <$ many1' (satisfy (/='\n'))
                        condDesc = do
                          cond <- single '?' >> visible ""
                          desc <- skipMany' nbhspace >> many' (do x <- fill Nothing eol <+? map Just token
                                                                  maybe zero return x)
                          return [FlagDescription cond desc]
                        localOpt = map2 (Just s,)
                        condClause = (<+? clause) $ do
                          single '+'
                          (exc,inc) <- delDefault . partitionEithers <$> sepBy1' (option' Right (Left <$ single '!') <*> visible ",") (single ',')
                          let cond = Conditional inc exc
                          hspace >> condClause <&> map (second cond)
                        base = init $ dropFileName s
                        include = do
                          several "include" >> nbhspace
                          p <- sepBy' (visible "=") nbhspace <* hspace <* single '=' <* hspace
                          ts <- lift . fileArgs (mnt+p) =<< map (base</>) (visible "")
                          return (ts <&> l'2.t'Mount %~ ((p+) <#> t'Source.l'1 %~ (p+)))
                        echo = several ">" >> pure . Target . Echo base<$>option' "" (nbhspace >> many' (satisfy (/='\n')))
                        exe = single '%' >> nbhspace >> foldl1' (<+?) [cmdLine f | Option _ ["execute"] (ReqArg f _) _ <- curlyOpts]
                        flag = several "flag" >> nbhspace >> pure . Flag <$> visible ""
                        longPrelude = do
                          several "prelude" >> hspace >> eol
                          let getLines x = (x <$ lookingAt (several "end prelude"))
                                           <+? (do l <- many' (satisfy (/='\n')) <* eol
                                                   getLines (l:x))
                          map (Target . AddPrelude) . reverse <$> getLines []
                        inp = several "mount" 
                        tgt = several "target" + single '-'
                        cmd "mount" _ = inp >> nbhspace >> pure . uncurry Mount<$>inputSource base
                        cmd n (NoArg x) = tgt >> nbhspace >> x <$ several n
                        cmd n (ReqArg f _) = tgt >> nbhspace >> several n >> nbhspace >> cmdLine f
                        cmd n (OptArg f _) = tgt >> nbhspace >> several n >> nbhspace >> option' (f Nothing) (cmdLine (f . Just))
                        cmdLine f = map adjustTgt . f . intercalate " "<$>sepBy1' (visible "") nbhspace
                        adjustTgt = t'Target.targetFilepaths %~ (base</>)

withCurlyPlex :: MonadIO m => CurlyConfig -> ((?curlyPlex :: CurlyPlex) => m a) -> m a
withCurlyPlex opts x = do
  cp <- liftIO $ curlyPlex opts
  let ?curlyPlex = cp in x

curlyPlex :: CurlyConfig -> IO CurlyPlex
curlyPlex args = do
  ret <- composing addOpt (snd<$>args) <$> newCurlyPlex
  let ?curlyPlex = ret in reloadMountain
  return ret
  where addOpt (Mount p s) = mounts %~ (+[(p,s)])
        addOpt (Target t) = targets %~ (+[t])
        addOpt (Conditional inc exc o) | isValidCond flags inc exc = addOpt o
        addOpt _ = id
        isValidCond flags inc exc = (nonempty (inc*flags) || empty inc) && empty (exc*flags)
        allFlags cur | cur==next = cur
                     | otherwise = allFlags next
          where next = composing addFlag (snd<$>args) cur
                addFlag (Flag f) = touch f
                addFlag (Conditional inc exc o) | isValidCond cur inc exc = addFlag o
                addFlag _ = id
        flags = allFlags (touch "command"
                          $ (if all (has t'setting) [t | (Nothing,Target t) <- args]
                                && not (or [True | (_,Flag _) <- args])
                             then touch "default" else id)
                          $ (c'set.fromKList) [f | (_,Flag f) <- args])

curlyFiles :: CurlyConfig -> [FilePath]
curlyFiles args = foldr go (const []) [s | (Just s,_) <- args] (c'set zero)
  where go f fs visited | isKeyIn f visited = fs visited
                        | otherwise = f:fs (touch f visited)

sourceLibs :: (?mountain::Mountain, ?curlyPlex :: CurlyPlex) => [([String],FileLibrary)]
sourceLibs = symList $ fromPList [(p,sourceFile b (f,c) (getFile f^.thunk)) | (p,Source b f c) <- ?curlyPlex^.mounts]
  where symList (Pure l) = [([],l)]
        symList (Join (ModDir l)) = join [symList a <&> l'1 %~ (s:) | (s,a) <- l]

curlyHistoryFile :: String
curlyHistoryFile = curlyUserDir </> "history"

getVCSBranches :: MonadIO m => String -> m Branches
getVCSBranches name = liftIO $ do
  conn <- readIORef libraryVCS
  u <- lookup name <$> getKeyStore
  case u of
    Just (_,pub,_,_,_) -> getBranches conn pub
    _ -> return zero
