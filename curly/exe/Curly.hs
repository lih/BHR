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
import Curly.Core.Peers
import Curly.Core.VCS
import Curly.Session
import Curly.System
import Curly.System.Base
import Curly.UI
import Curly.UI.Options hiding (nbsp,spc)
import Curly.Core.Parser
import Data.IORef
import GHC.Conc (threadDelay)
import GHC.IO.Encoding (utf8,setLocaleEncoding)
import IO.Filesystem
import IO.Network.Socket
import Language.Format
import System.Environment (getArgs)
import System.Process (readProcess)
import System.IO (withFile,IOMode(..))

main :: IO ()
main = cli "curly" $ do
  initCurly
  
  args <- parseCurlyArgs <$> getArgs
  let ?commandLineScripts = [s | Left s <- args]
  withCurlyConfig args $ withCurlyPlex ?curlyConfig $ do
    let uninhibited = not ( any (has t'Help) (?curlyPlex^.targets)
                            || all (has t'setting) (?curlyPlex^.targets) )
        tgts = try [Help] (guard uninhibited >> ?curlyPlex^.targets)
    runTargets tgts

data TargetType = ForkTgt (MVar ())
                | IOTgt (IO ())

t'IOTgt :: Traversal' TargetType (IO ())
t'IOTgt k (IOTgt m) = IOTgt<$>k m
t'IOTgt _ x = return x

initCurly = setLocaleEncoding utf8

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
runTarget (Echo base x) = ioTgt $ do
  let echoLine = many' (Pure <$> many1' (noneOf "${}")
                        <+? splice)
      splice = between (several "${") (single '}') (Join <$> echoLine)
      runEchoLine (Pure s) = return s
      runEchoLine (Join l) = do
        s' <- fold <$> traverse runEchoLine l
        case words s' of
          "run":p:args -> readProcess p args ""
          "path":[p] -> return (base</>p)
          "env":[v] -> return (envVar "" v)
          "env":[v,def] -> return (envVar def v)
          cmd -> error $ format "Unrecogized command '%s'" (intercalate " " cmd)
  case matches Just echoLine x of
    Just el -> traverse runEchoLine el >>= putStrLn . fold
    Nothing -> error "Invalid echo line"
       
runTarget Help = ioTgt $ do
  let argFlags (Conditional i e arg) = i+e+argFlags arg
      argFlags (FlagDescription n _) = singleton' n
      argFlags _ = zero
      extraFlags = [(n,d) | (_,FlagDescription n d) <- ?curlyConfig]
      allFlags = foldMap (argFlags . snd) ?curlyConfig
      flagDesc | empty allFlags = "OPTION"
               | otherwise = format "(OPTION%s)" $ foldMap ("|+"+) allFlags
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
  putStrLn $ format "Repositories%s:" (valOrigin "CURLY_PATH")
  repos <- readIORef repositories
  for_ repos $ \r -> putStrLn $ "  * "+show r
  putStrLn $ format "Library cache: %s%s" curlyCacheDir (valOrigin "CURLY_LIBCACHE")
  putStrLn $ format "Server port: %p%s"   curlyPort     (valOrigin "CURLY_PORT")
  putStrLn $ format "Version control: %s%s" (show curlyVCSBackend) (valOrigin "CURLY_VCS")
  putStrLn $ format "Publisher key: %s%s" curlyPublisher (valOrigin "CURLY_PUBLISHER")
  putStr "\n"
  putStrLn $ format "Mounts:%s" (if nonempty (?curlyPlex^.mounts) then "" else " none")
  for_ (?curlyPlex^.mounts) $ putStrLn . uncurry (format "  * %s = %I" . intercalate " ")
  let showTgts = c'list $ select (/=Help) $ ?curlyPlex^.targets
  putStrLn $ format "Targets:%s" (if nonempty showTgts then "" else " none")
  traverse_ (putStrLn . format "  * %T") showTgts
runTarget Interactive = ioTgt $ runCurlySession (\_ -> unit) LocalClient =<< targetServer
runTarget (Execute s) = ioTgt $ runCurlySession (\_ -> unit) (StringClient s) =<< targetServer
runTarget (RunFile f) = ioTgt $ runCurlySession (\_ -> unit) (FileClient f) =<< targetServer
runTarget (Server InstanceServer) = forkTgt $ \tid1 -> trylog unit $ bracket_
                                                       (modifyIORef processInstances (touch (getConf confInstance)))
                                                       (modifyIORef processInstances (delete (getConf confInstance)))
                                                       $ do
  port <- until $ (<*= maybe (threadDelay 1000000) (const unit)) $ do
    h <- peerClient
    runConnection Just True h $ do
      x <- exchange (DeclareInstance (getConf confInstance))
      case x of
        Left err -> liftIO (putStrLn err) >> zero
        Right p -> return p
  tid2 <- forkIO $ while $ catch (\e -> case fromException e of
                                    Just ThreadKilled -> return False
                                    _ -> print e >> return True)
          $ map (const True) $ do
    h <- peerClient
    runConnection_ True h $ doTimes_ 1000 $ void $ exchange (RedeclareInstance (getConf confInstance) port)
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
    

runTarget (Server LibServer) = forkTgt $ \_ -> do
  putStrLn $ format "Serving libraries on port %p" curlyPort
  forever $ do
    h <- peerClient
    runConnection_ True h $ do
      send DeclareSupply
      forever $ receive >>= liftIO . \x -> withMountain $ do
        let localLibs = c'map (fromAList [(fl^.flID,(p,fl^.flLibrary,fl^.flBytes)) | (p,fl) <- sourceLibs])
        ls <- case x of
          Nothing -> availableLibs <&> \al -> SupplyLibraries [(i,l^.metadata)
                                                              | (i,Just l) <- (localLibs^.ascList <&> \(i,(_,l,_)) -> (i,Just l))
                                                                              + (al <&> \(i,_) -> (i,map (by flLibrary) $ findLib i))]
          Just l -> return (SupplyLibrary $ fromMaybe zero (map (by l'3) (localLibs^.at l) + map (by flBytes) (findLib l)))
        send ls
runTarget (ListServer LibServer t) = ioTgt $ showLibs =<< availableLibs 
  where showLibs l = traverse_ showLib [(l,d) | (l,Just d) <- map (second showT) l]
        showLib (l,d) = putStrLn (show l+" "+d)
        showT d = maybe (Just $ pretty d) (showTemplate d) t
runTarget (ListServer InstanceServer _) = ioTgt $ do
  let (h,p) = case getConf confServer of
        Just (_,h,p) -> (h,p)
        Nothing -> ("127.0.0.1",curlyPort)
  h <- connectTo h p
  insts <- fold <$> runConnection Just True h (exchange AskInstances)
  putStrLn $ format "Available instances: %s" (intercalate "," insts)

runTarget (ShowLib l) = ioTgt $ do
  let libName = Right <$> single '@' *> readable <* eoi
                <+? Left <$> remaining
      Just nm = matches Just libName l
  case nm of
    Left file -> do
      l' <- matches Just datum <$> readBytes file
      case l' of
        Just x -> print (x :: Library)
        Nothing -> withMountain $ do
          Id l' <- parseCurly<$>readString file<*>pure curlyFile
          case l' of
            Right x -> print x
            Left ws -> putStrLn $ format "Couldn't parse source file:%s"
                       $ foldMap (("\n  "+) . showWarning (Just file)) ws
    Right i -> case findLib i of
       Just x -> print (x^.flLibrary)
       Nothing -> putStrLn $ "Couldn't find library "+show i
runTarget (Translate f sys path) = ioTgt $ withMountain $ case localContext^?atMs path of
  Just (Pure (_,e)) -> do
    let prog = specializeStandalone sys e
    createDirectoryIfMissing True (dropFileName f)
    withFile f WriteMode $ \h -> writeHBytes h prog
    modifyPermissions f (_sysProgPerms sys)
  _ -> putStrLn $ "Error: the path "+show path+" doesn't seem to point to a function in the default context"
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