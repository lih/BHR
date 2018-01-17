{-# LANGUAGE CPP #-}
module Curly.UI.Options (
  -- * The global mode of operation
  CurlyPlex(..),mounts,targets,mountainCache,jitContext,newCurlyPlex,

  -- * Inputs and targets
  CurlyOpt(..),InputSource(..),ServerType(..),Target(..),
  t'Mount,t'Target,t'Source,t'Help,t'setting,targetFilepaths,readServer,showOpts,

  -- * Per-target threaded configuration
  TargetParams,confServer,confPrelude,confBanner,confInstance,confThreads,defaultConf,getConf,withPrelude,

  -- * Misc
  curlyOpts,inputSource,curlyFileName,noCurlySuf,visible,nbsp,spc
  ) where

import Definitive
import Curly.Core
import Curly.Core.Library
import Language.Syntax.CmdArgs
import Curly.System.Base (System)
import Curly.System 
import Control.Concurrent (ThreadId)
import Data.IORef (IORef,newIORef)

data CurlyOpt = Mount [String] InputSource
              | Target Target
              | Flag String
              | FlagDescription String String
              | Conditional (Set String) (Set String) CurlyOpt
              deriving (Eq,Ord,Show)
t'Mount :: Traversal' CurlyOpt ([String],InputSource)
t'Mount k (Mount s i) = k (s,i) <&> \(s',i') -> Mount s' i'
t'Mount _ x = pure x
t'Target :: Traversal' CurlyOpt Target
t'Target k (Target t) = Target<$>k t
t'Target _ x = pure x

data InputSource = Source [String] String String
                 | Library LibraryID
                 | LibraryFile String
                 deriving (Eq,Ord)
instance Show InputSource where
  show (Source p s c) = "source"+showSub+" "+s+" "+c
    where showSub | empty p = ""
                  | otherwise = "["+intercalate " " p+"] "
  show (Library i) = "library "+show i
  show (LibraryFile f) = "library @"+f
instance FormatArg InputSource where argClass _ = 'I'

t'Source :: Traversal' InputSource ([String],String,String)
t'Source k (Source b s c) = k (b,s,c) <&> \(b',s',c') -> Source b' s' c'
t'Source _ x = pure x

data ServerType = LibServer | InstanceServer
               deriving (Eq,Ord)
instance Show ServerType where show LibServer = "libraries" ; show InstanceServer = "instances"
instance Read ServerType where
  readsPrec _ = readsParser (LibServer<$like "libraries" <+? InstanceServer<$like "instances")

type InstanceName = String
type HostName = String

data Target = Help | Version 
            | Interactive
            | Execute String
            | RunFile FilePath
            | Server ServerType
            | ListServer ServerType (Maybe Template)
            | ShowLib FilePath
            | DumpDataFile (Maybe FilePath)
            | SetPrelude String
            | AddPrelude String
            | SetBanner String
            | AddBanner String
            | SetServer (Maybe (InstanceName,HostName,PortNumber))
            | SetInstance InstanceName
            | Echo FilePath String
            | Translate FilePath System [String]
            deriving (Eq,Ord)
instance Show Target where
  show Help = "help"
  show Version = "version"
  show Interactive = "interactive"
  show (Execute s) = "execute "+s
  show (RunFile s) = "run "+s
  show (Server t) = "serve "+show t
  show (ListServer t tpl) = "list "+show t+" "+maybe "" pretty tpl
  show (ShowLib l) = "dump "+l
  show (SetPrelude p) = "prelude  "+p
  show (AddPrelude p) = "prelude+ "+p
  show (SetBanner b) = "banner  "+b
  show (AddBanner b) = "banner+ "+b
  show (SetServer Nothing) = "at local"
  show (SetServer (Just (i,h,p))) = format "at %s:%p/%s" h p i
  show (SetInstance i) = "instance "+i
  show (Echo _ s) = "echo "+s
  show (Translate f s p) = format "translate %s @ %s = %s" f (show s) (intercalate " " p) 
  show (DumpDataFile f) = "dump-data-file "+fromMaybe "" f
  
instance FormatArg Target where argClass _ = 'T'
t'Help :: Traversal' Target ()
t'Help k Help = Help <$ k ()
t'Help _ x = return x
t'setting :: Traversal' Target ()
t'setting k x@(SetInstance _) = x <$ k ()
t'setting k x@(SetPrelude _) = x <$ k ()
t'setting k x@(AddPrelude _) = x <$ k ()
t'setting k x@(SetServer _) = x <$ k ()
t'setting k x@(SetBanner _) = x <$ k ()
t'setting k x@(AddBanner _) = x <$ k ()
t'setting _ x = return x
targetFilepaths :: Traversal' Target String
targetFilepaths k (ShowLib l) = ShowLib<$>k l
targetFilepaths k (Translate n s p) = k n <&> \n' -> Translate n' s p
targetFilepaths k (RunFile f) | f/="-" = RunFile<$>k f
targetFilepaths _ t = return t

data TargetParams = TargetParams {
  _confServer :: Maybe (String,String,PortNumber),
  _confBanner :: String,
  _confPrelude :: [String],
  _confInstance :: String,
  _confThreads :: Maybe (ThreadId,ThreadId,IORef (Int,Map Int ThreadId))
  }
confServer :: Lens' TargetParams (Maybe (String,String,PortNumber))
confServer = lens _confServer (\x y -> x { _confServer = y })
confPrelude :: Lens' TargetParams [String]
confPrelude = lens _confPrelude (\x y -> x { _confPrelude = y })
confBanner :: Lens' TargetParams String
confBanner = lens _confBanner (\x y -> x { _confBanner = y })
confInstance :: Lens' TargetParams String
confInstance = lens _confInstance (\x y -> x { _confInstance = y })
confThreads :: Lens' TargetParams (Maybe (ThreadId,ThreadId,IORef (Int,Map Int ThreadId)))
confThreads = lens _confThreads (\x y -> x { _confThreads = y })

defaultBanner :: String
defaultBanner = unlines [
  "+---------------------------------------------------------------------------+",
  "|      __     __ _ __ ___   __  _  __                                       |",
  "|   __/  ` ,'_//// // o | / / | |/,'   Welcome to the Curly Shell !         |",
  "|  / /\\   / /_/ U //  ,' / /_ | ,'                                          |",
  "| |  \\/   |__/\\_,'/_/`_\\/___//_/       Enter 'help' if you need it          |",
  "| \\                           v"+VERSION_curly+drop (length VERSION_curly)
                                 "                                             |",
  "+---------------------------------------------------------------------------+"
  ]
defaultConf :: TargetParams
defaultConf = TargetParams zero defaultBanner zero "curly" zero
getConf :: (?targetParams :: TargetParams) => Lens' TargetParams a -> a
getConf l = ?targetParams ^. l
withPrelude :: (?targetParams :: TargetParams) => String -> String
withPrelude s = getConf confPrelude & \p -> intercalate "\n" (p+[s])

curlyOpts = [
  Option ['h'] ["help"] (NoArg (target Help)) "Displays the help menu and some basic information (inhibits other flags)",
  Option ['v'] ["version"] (NoArg (target Version)) "Shows the current Curly version",
  sepOpt "Inputs",
  Option ['M'] ["mount"] (ReqArg mkMount "PATH=MOUNT") "Mounts an input source to a path in the default context",
  sepOpt "Target configuration ",
  Option ['P'] ["prelude"] (ReqArg (target . SetPrelude) "COMMAND") "Sets the prelude for the next targets",
  Option ['p'] ["prelude+"] (ReqArg (target . AddPrelude) "COMMAND") "Appends the given command to the prelude",
  Option [] ["banner"]  (ReqArg (target . SetBanner) "BANNER") "Sets the banner for the next targets",
  Option [] ["banner+"]  (ReqArg (target . AddBanner) "BANNER") "Adds a line to the banner file for the next targets",
  Option [] ["instance"] (ReqArg (target . SetInstance) "INSTANCE") "Sets the instance name for the next targets",
  Option [] ["at"] (ReqArg (target . SetServer . readServer) "[SERVER]/INSTANCE") "Sets the server for the next targets",
  sepOpt "Sessions",
  Option ['i'] ["interactive"] (NoArg (target Interactive)) "Launches an interactive session",
  Option ['e'] ["execute"]     (ReqArg (target . Execute) "COMMAND") "Executes an interactive command",
  Option ['r'] ["run"]    (ReqArg (target . RunFile) "FILE") "Runs interactive commands from the given file, or stdin if the file is -",
  sepOpt "Distribution",
  Option ['s'] ["serve"] (ReqArg (target . Server . readServerType) "SERVER_TYPE") "Launches a new library (type 'libraries') or instance (type 'instances') server",
  Option ['l'] ["list"] (ReqArg (target . readListServer) "SERVER_TYPE") "Lists all available libraries in the CURLY_PATH or instances on the current server",
  sepOpt "Files",
  Option ['t'] ["translate"] (ReqArg (target . mkTranslate) "FILE[@SYS][=PATH]") "Translates a Curly function for a system",
  Option ['d'] ["dump"] (ReqArg (target . ShowLib) "FILE") "Shows the contents of the given source or library file",
  Option [] ["dump-data-file"] (OptArg (target . DumpDataFile) "FILE") "Dumps the contents of an installed data file. Without arguments, lists the files that can be dumped."
  ]
  where tryParse err p s = fromMaybe (error (err s)) (matches Just p s)
        mkMount = tryParse (format "Couldn't parse mount option '%s'") (inputSource "." <&> pure . uncurry Mount)
        mkTranslate = tryParse (format "Coudn't parse translate option '%s' (expected FILE=MODULE:...:FUNCTION)") translate
        sepOpt dsc = Option [] [] (NoArg (target Help)) ("--- " + dsc + " ---")
        target t = [Target t]
        readServerType = tryParse (format "Couldn't parse server type '%s' (expected 'libraries' or 'instances')") readable
        readListServer = tryParse (format "Couldn't parse server type '%s' (expected 'libraries' or 'instances')") $ do
          t <- readable
          tpl <- option' Nothing (Just <$> docLine "template" [])
          return (ListServer t tpl)
          
        translate = do
          n <- visible "@=" <* spc
          s <- option' hostSystem (single '@' >> spc >> visible "="
                                  >>= \s -> maybe zero return (knownSystems^.at s))
          let splitPath ('.':t) = "":splitPath t
              splitPath ('-':t) = "":splitPath t
              splitPath (c:t)   = let ~(h:t') = splitPath t in (c:h):t'
              splitPath [] = [""]
          p <- option' (splitPath n) $ do
            between spc spc $ single '=' 
            sepBy1' (visible "") nbsp
          return (Translate n s p)

grid :: String -> [[String]] -> String
grid sep l = unlines $ map (intercalate sep . zipWith padTo lengths) l
  where padTo n s | length s>=n = s
                  | otherwise = s+take (n-length s) (repeat ' ')
        lengths = mk l
          where mk (h:t) = zipWith max (map length h+repeat 0) (mk t)
                mk [] = repeat 0

showOpts opts = grid "  " $ [
  [intercalate "," (map (\x -> ['-',x]) short),intercalate "," (map ("--"+) long)
  ,case arg of
     ReqArg _ a -> a
     _ | empty short && empty long -> ""
       | otherwise -> "(nothing)"
  ,desc]
  | Option short long arg desc <- opts]

readServer s = matches Just (
  liftA3 (\h p i -> (i,h,p))
  (option' "127.0.0.1" $ many1' (noneOf "/:"))
  (option' 25465 (single ':' >> number))
  (single '/' >> remaining)) s

isSpc = (`elem`": \t")
nbsp,spc :: (ParseStream Char s,Monad m) => ParserT s m ()
nbsp = skipMany1' (satisfy isSpc)
spc = option' () nbsp 
visible :: (ParseStream Char s,Monad m) => [Char] -> ParserT s m String
visible lim = many1' (satisfy (not . \c -> isSpc c || c=='\n' || c`elem`lim))

inputSource base = do
  p <- sepBy' (visible "=") nbsp
  between spc spc (several "=")
  (p,) <$> (src <+? lib <+? search <+? blts)
  where src = do
          like "source"
          sub <- option' [] (between (single '[') (single ']')
                             $ between spc spc
                             $ sepBy' (visible "]") nbsp)
          nbsp
          n <- visible ""
          let defaultCache = fromMaybe (n+".cache") (noCurlySuf n <&> (+".cyl"))
          m <- option' defaultCache (nbsp >> visible "")
          return (Source sub (base</>n) (base</>m))
        search = like "package" >> nbsp >> do
          let tag x l = Join (DocTag x [] l)
          tpl <- (docAtom <*= guard . has t'Join)
                 <+? (visible "" <&> \x -> tag "=" [tag "$" [Pure "name"],Pure x])
          let sid = availableLibs
                    <&> \ls -> fromMaybe (error $ format "Could not find package matching %s" (pretty tpl))
                               $ find (\(_,d) -> nonempty (showTemplate d tpl)) ls <&> fst
          return (Library $ sid^.thunk)
        lib = like "library" >> nbsp >> (fileLib <+? map Library readable)
          where fileLib = single '@' >> map LibraryFile (visible "")
        blts = Library (builtinsLib^.flID) <$ like "builtins"
        
data CurlyPlex = CurlyPlex {
  _mounts :: [([String],InputSource)],
  _targets :: [Target],
  _mountainCache :: IORef (Mountain,[Mountain -> IO ()]),
  _jitContext :: JITContext GlobalID
  }
mounts :: Lens' CurlyPlex [([String],InputSource)]
mounts = lens _mounts (\x y -> x { _mounts = y })
targets :: Lens' CurlyPlex [Target]
targets = lens _targets (\x y -> x { _targets = y })
mountainCache :: Lens' CurlyPlex (IORef (Mountain,[Mountain -> IO ()]))
mountainCache = lens _mountainCache (\x y -> x { _mountainCache = y })
jitContext :: Lens' CurlyPlex (JITContext GlobalID)
jitContext = lens _jitContext (\x y -> x { _jitContext = y })

curlyFileName s = fromMaybe (0 :: Int,"") $ matches Just axiom s
  where axiom = liftA2 (,) (option 0 num) tl 
        num = number <* satisfy (`elem`".-: ")
        tl = remaining

newCurlyPlex = CurlyPlex [] [] <$> newIORef zero <*> newJITContext
