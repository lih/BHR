{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns #-}
module Main(
  -- * The main Curly function
  main,

  -- * Functions for running your own Curly instances
  initCurly,runTargets,
  ) where

import Control.Concurrent (forkIO,forkFinally)
import Control.Concurrent.MVar (putMVar,takeMVar,newEmptyMVar,MVar)
import Control.Exception (AsyncException(..),Exception(..),bracket_)
import Curly.Core
import Curly.Core.Library
import Curly.Core.Parser
import Curly.Core.Peers
import Curly.Core.Security
import Curly.Core.VCS
import Curly.Core.VCS.Diff (patch)
import Curly.Session
import Curly.System
import Curly.System.Base
import Curly.UI
import Curly.UI.Options hiding (nbsp,spc)
import Data.IORef
import GHC.Conc (threadDelay)
import GHC.IO.Encoding (utf8,setLocaleEncoding)
import IO.Filesystem
import IO.Network.Socket
import IO.Time (currentTime)
import Language.Format
import Paths_curly
import System.Directory (doesFileExist,getCurrentDirectory)
import System.Environment (getArgs)
import System.IO (withFile,IOMode(..))
import System.Process (readProcess)
import System.FilePath.Posix (splitFileName)

main :: IO ()
main = cli "curly" $ do
  initCurly

  cwd <- getCurrentDirectory
  let prefixes "" = []
      prefixes f = let (h,t) = splitFileName f in f:prefixes (init h)
  args <- parseCurlyArgs <$> getArgs
  let defaultConfig = curlyUserDir</>"default.curly"
  do ex <- doesFileExist defaultConfig
     unless ex $ do
       createFileDirectory defaultConfig
       txt <- (readString =<< curlyDataFileName "default.curly")
       writeString defaultConfig txt
  additional <- map (\x -> [defaultConfig] + convert x)
                (firstExistingFile (if any (has t'1) args then [] else map (+"/.curly") (prefixes cwd)))
  
  let fullArgs = map Left additional + args

  let ?commandLineScripts = [s | Left s <- fullArgs]
  withCurlyConfig fullArgs $ withCurlyPlex ?curlyConfig $ do
    let uninhibited = not ( any (has t'Help) (?curlyPlex^.targets)
                            || all (has t'setting) (?curlyPlex^.targets) )
        tgts = try [Help] (guard uninhibited >> ?curlyPlex^.targets)
    runTargets tgts

firstExistingFile :: [String] -> IO (Maybe String)
firstExistingFile fs = foldr go (return Nothing) fs
  where go f rest = do
          ex <- doesFileExist f
          if ex
            then return (Just f)
            else rest

data TargetType = ForkTgt (MVar ())
                | IOTgt (IO ())

t'IOTgt :: Traversal' TargetType (IO ())
t'IOTgt k (IOTgt m) = IOTgt<$>k m
t'IOTgt _ x = return x

initCurly = do
  setLocaleEncoding utf8
  putMVar getDataFileName_ref getDataFileName
  curlyDataFileName "proto/vc" >>= \p -> modifyIORef vcsProtoRoots (p:)
  
ioTgt = return . IOTgt
forkTgt m = do
  v <- newEmptyMVar
  mfix $ \tid -> forkFinally (m tid) (\_ -> putMVar v ())
  return (ForkTgt v)

runTargets :: (?commandLineScripts :: [String], ?curlyConfig :: CurlyConfig, ?curlyPlex :: CurlyPlex, ?programName :: String) => [Target] -> IO ()
runTargets targetList = do
  let withParams tgt cfg = (nextParams tgt cfg,
                            let ?targetParams = cfg in do
                              let tgtHead = logLine Verbose $ format "Running target: %T" tgt
                              runTarget tgt <&> t'IOTgt %~ (tgtHead >> ))
  tgts <- sequence $ mapAccum_ withParams targetList defaultConf
  sequence_ [catch print io | IOTgt io <- tgts]
  sequence_ [takeMVar v | ForkTgt v <- tgts]

runTarget :: (?commandLineScripts :: [String], ?curlyConfig :: CurlyConfig, ?curlyPlex :: CurlyPlex, ?programName :: String,?targetParams :: TargetParams) => Target -> IO TargetType
runTarget Version = ioTgt $ putStrLn $ format "Curly, version %s. Crafted with love by Marc O. Coiffier." VERSION_curly
runTarget (Echo x) = ioTgt $ putStrLn x

runTarget Help = ioTgt $ do
  let argFlags (Conditional i e (CurlyCondOpt arg)) = keysSet i+e+argFlags (arg zero)
      argFlags (FlagDescription n _) = singleton' n
      argFlags _ = zero
      extraFlags = [(n,d) | (_,FlagDescription n d) <- ?curlyConfig]
      allFlags = foldMap (argFlags . snd) ?curlyConfig
      flagDesc | empty allFlags = "OPTION"
               | otherwise = format "(OPTION%s)" $ foldMap ("|+"+) allFlags
      indent i s = unlines (map (i+) (lines s))
  putStrLn $ format "Usage: %s%s %s..."
    ?programName (foldMap (' ':) ?commandLineScripts)
    flagDesc
  putStrLn $ indent "  " $ showOpts curlyOpts

  unless (empty extraFlags) $ do
    putStrLn "Extra options"
    sequence_ [putStrLn $ format "  +%s: %s" n d | (n,d) <- extraFlags]
    putStrLn ""
  
  let valOrigin v = c'string $ case envVar "" v of
        "" -> format " (default value, set %s to override)" v
        _ -> format " (from %s)" v
  
  putStrLn $ "Known systems: "+intercalate ", " (map show knownSystems)
  putStrLn $ format "Repositories%s:" (valOrigin "CURLY_VCS")
  VCSB_Native repos _ _ <- readIORef libraryVCS
  for_ repos $ \r -> putStrLn $ "  * "+r
  putStrLn $ format "Library cache: %s%s" curlyCacheDir (valOrigin "CURLY_LIBCACHE")
  putStrLn $ format "Server port: %p%s"   curlyPort     (valOrigin "CURLY_PORT")
  putStrLn $ format "Publisher key: %s%s" curlyPublisher (valOrigin "CURLY_PUBLISHER")
  putStr "\n"
  putStrLn $ format "Mounts:%s" (if nonempty (?curlyPlex^.mounts) then "" else " none")
  for_ (?curlyPlex^.mounts) $ putStrLn . uncurry (format "  * %s = %I" . intercalate ".")
  let showTgts = c'list $ select (/=Help) $ ?curlyPlex^.targets
  putStrLn $ format "Targets:%s" (if nonempty showTgts then "" else " none")
  traverse_ (putStrLn . format "  * %T") showTgts
runTarget Interactive = ioTgt $ runCurlySession (\_ -> unit) LocalClient =<< targetServer
runTarget (Execute s) = ioTgt $ runCurlySession (\_ -> unit) (StringClient s) =<< targetServer
runTarget (RunFile f) = ioTgt $ runCurlySession (\_ -> unit) (FileClient f) =<< targetServer
runTarget ServeInstance = forkTgt $ \tid1 -> trylog unit $ bracket_
                                             (modifyIORef processInstances (touch (getConf confInstance)))
                                             (modifyIORef processInstances (delete (getConf confInstance)))
                                             $ do
  port <- until $ (<*= maybe (threadDelay 1000000) (const unit)) $ do
    h <- peerClient
    runConnection Just True h $ do
      x <- exchange (DeclareInstance (getConf confInstance))
      case x of
        Left err -> liftIO (putStrLn err) >> zero
        Right (PeerPort p) -> return p
  tid2 <- forkIO $ while $ catch (\e -> case fromException e of
                                    Just ThreadKilled -> return False
                                    _ -> print e >> return True)
          $ map (const True) $ do
    h <- peerClient
    runConnection_ True h $ doTimes_ 1000 $ void $ exchange (RedeclareInstance (getConf confInstance) (PeerPort port))
  tids <- newIORef (c'int 0,c'map zero)
  let ?targetParams = ?targetParams & confThreads %- Just (tid1,tid2,tids)
      
  putStrLn $ format "Hosting instance '%s' on port %p" (getConf confInstance) port
  sock <- listenOn port
  forever $ do
    (h,addr) <- accept sock
    logLine Verbose $ "Accepting connection from "+show addr
    n <- runAtomic tids $ l'1`swapWith`(+1)
    (`forkFinally` (\e -> case e of
                      Left exc -> logLine Debug (show exc)
                      _ -> runAtomic tids $ do l'2 =~ delete n)) $ do
      let storeTid tid = runAtomic tids $ do l'2 =~ insert n tid
      runCurlySession storeTid (SocketClient h) =<< targetServer
    
runTarget ListInstances = ioTgt $ do
  let (h,p) = case getConf confServer of
        Just (_,h,p) -> (h,p)
        Nothing -> ("127.0.0.1",curlyPort)
  h <- connectTo h p
  insts <- fold <$> runConnection Just True h (exchange AskInstances)
  putStrLn $ format "Available instances: %s" (intercalate "," insts)

runTarget (Translate f sys path) = ioTgt $ do
  runAtomic (?curlyPlex^.mountainCache) $ do l'2 =~ (doBuild:)
  withMountain $ doBuild ?mountain
  where doBuild m = let ?mountain = m in case localContext^?atMs path of
          Just (Pure (_,e)) -> do
            let prog = specializeStandalone sys e
            createDirectoryIfMissing True (dropFileName f)
            withFile f WriteMode $ \h -> writeHBytes h prog
            modifyPermissions f (_sysProgPerms sys)
          _ -> putStrLn $ "Error: the path "+show path+" doesn't seem to point to a function in the default context"
  
runTarget (Goody "builtins/ids") = ioTgt $ for_ (reverse builtinLibs) $ \l -> putStrLn (show (l^.flID))
runTarget (Goody ('b':'u':'i':'l':'t':'i':'n':'s':'/':'v':x)) = ioTgt $ do
  let (h,ext) = splitAt (length x-4) x
  case ext of
    ".cyl" -> writeHBytes stdout ((reverse builtinLibs!!(read h-1))^.flBytes)
    _ -> error $ "No such builtin library: "+x
runTarget (Goody "list") = ioTgt $ do
  fn <- curlyDataFileName "list"
  readBytes fn >>= writeHBytes stdout
  putStrLn "builtins/ids"
  for_ (zip [1..] builtinLibs) $ \(i,l) -> do
    putStrLn $ "builtins/v"+show i
runTarget (Goody f) = ioTgt $ do
  fn <- curlyDataFileName f
  readBytes fn >>= writeHBytes stdout
runTarget (SetServer _) = ioTgt unit
runTarget (SetPrelude _) = ioTgt unit
runTarget (AddPrelude _) = ioTgt unit
runTarget (SetBanner _) = ioTgt unit
runTarget (AddBanner _) = ioTgt unit
runTarget (SetInstance _) = ioTgt unit

nextParams (SetServer srv) = confServer %- srv
nextParams (SetPrelude "") = confPrelude %- []
nextParams (SetPrelude i) = confPrelude %- [i]
nextParams (AddPrelude i) = confPrelude %~ (+[i])
nextParams (SetBanner b) = confBanner %- (b+"\n")
nextParams (AddBanner b) = confBanner %~ (+b+"\n")
nextParams (SetInstance i) = confInstance %- i
nextParams _ = id

