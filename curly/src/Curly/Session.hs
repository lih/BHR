{-# LANGUAGE CPP, ScopedTypeVariables, RecursiveDo, DeriveGeneric #-}
module Curly.Session (
  -- * Running sessions
  ClientEndPoint(..),ServerEndPoint(..),runCurlySession,targetServer
  ) where

import Control.Concurrent.Chan
import Control.Concurrent (forkIO,killThread,ThreadId)
import Control.Concurrent.MVar
import Control.DeepSeq (($!!),deepseq)
import Curly.Core
import Curly.Core.Library
import Curly.Core.Parser
import Curly.Core.Peers
import Curly.Core.Security
import Curly.Core.VCS
import Curly.Session.Commands
import Curly.Style
import Curly.UI
import Curly.UI.Options hiding (nbsp,spc)
import Data.IORef 
import GHC.IO.Handle (hSetBuffering,hClose,BufferMode(..))
import IO.Filesystem
import Language.Format
import System.Console.Readline (readline,addHistory,setCompletionEntryFunction,getLineBuffer,setCompleterWordBreakCharacters)
import System.Directory (removeFile)
import System.Environment
import System.Exit (exitSuccess)
import System.IO (hIsTerminalDevice)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Temp (mkstemps)
import System.Process (callProcess,spawnProcess)

isPrefix [] _ = True
isPrefix (x:l) (y:l') = x==y && isPrefix l l'
isPrefix _ _ = False

data ClientPacket = BannerRequest Bool
                  | LineResponse String
                  | CompleteRequest String String
                  | EditResponse Bytes
                  | EndOfTransmission
                  | PubkeyResponse (Maybe PublicKey)
                  | KeyListResponse [(String,KeyFingerprint,Bool)]
                  deriving Generic
instance Serializable ClientPacket where
instance Format ClientPacket where
data ServerPacket = LineRequest
                  | CompleteResponse [String]
                  | CommandOutput Bytes
                  | EditRequest String (Int,Int) Bytes
                  | CommandAck
                  | PubkeyRequest String
                  | KeyGenRequest Bool String
                  | KeyListRequest
                  | ServerHasQuit
                  deriving Generic
instance Serializable ServerPacket where
instance Format ServerPacket where

data Connection = Connection {
  connClient :: Chan ClientPacket,
  connServer :: Chan ServerPacket
  }

data ClientEndPoint = StringClient String
                    | FileClient String
                    | LocalClient
                    | SocketClient Handle
data ServerEndPoint = LocalServer
                    | SocketServer Handle

serve :: Connection -> ServerPacket -> IO ()
serve conn = writeChan (connServer conn)

connGetContents :: Connection -> IO String
connGetContents (Connection clt srv) = do
  clt' <- dupChan clt
  fix $ \getC -> unsafeInterleaveIO $ do
    writeChan srv LineRequest
    (msg,ln) <- fix $ \getRes -> readChan clt' >>= \x -> case x of
      LineResponse ln -> return (x,ln)
      EndOfTransmission -> return (x,"")
      _ -> getRes
    lns <- case msg of EndOfTransmission -> return ""; _ -> getC
    return (ln + lns)

localEdit ext (l,c) b = do
  (tmp,htmp) <- mkstemps "edit-" ext
  hClose htmp
  writeBytes tmp b
  let p:as = words (envVar "vi" "EDITOR")
      lineCol = case p of
        "emacs" -> "+"+show (l+1)+":"+show (c+1)
        "emacsclient" -> "+"+show (l+1)+":"+show (c+1)
        "vim" -> "+normal "+show (l+1)+"G"+show (c+1)+"|"
        _ -> "+"+show (l+1)
  callProcess p (as + [lineCol, tmp])
  readBytes tmp <*= \bs -> bs`deepseq`removeFile tmp
  

runCurlySession :: (?curlyPlex :: CurlyPlex, ?curlyConfig :: CurlyConfig, ?targetParams :: TargetParams) => (ThreadId -> IO ()) -> ClientEndPoint -> ServerEndPoint -> IO ()
runCurlySession thr clt srv = (Connection<$>newChan<*>newChan) >>= \conn -> mdo
  let hasLocalClient = case clt of SocketClient _ -> False; _ -> True
  user <- case srv of LocalServer -> getConf confInstance<$withSessionState (localServer hasLocalClient thr acc conn)
                      SocketServer h -> handleServer h conn
  (acc,sem) <- case clt of
    LocalClient -> (Almighty,) <$> forkMVar (localClient user conn)
    SocketClient h -> handleClient (getConf confInstance) h conn
    StringClient s -> (Almighty,) <$> forkMVar (stringClient s conn)
    FileClient f -> (Almighty,) <$> forkMVar (fileClient f conn)
  takeMVar sem

localServer :: (?curlyPlex :: CurlyPlex, ?curlyConfig :: CurlyConfig, ?targetParams :: TargetParams, ?sessionState :: IORef SessionState)
               => Bool -> (ThreadId -> IO ()) -> Access -> Connection -> IO ()
localServer hasLocalClient thr acc conn@(Connection clt srv) = do
  forkIO watchSources
  start <- newEmptyMVar
  compPlex <- newIORef ?curlyPlex
  term <- setupTermFromEnv
  let ?terminal = term
  let getClientKeys = do
        clt <- dupChan (connClient conn)
        serve conn KeyListRequest
        until $ do
          x <- readChan clt
          return $ case x of
            KeyListResponse r -> Just r
            _ -> Nothing
  forkIO $ while $ do
    pkt <- readChan clt
    plex <- readIORef compPlex
    let ?curlyPlex = plex in case pkt of
      CompleteRequest lst ln -> withMountain $ do
        w <- getSession wd
        ks <- getKeyStore
        clientKeyNames <- unsafeInterleaveIO getClientKeys <&> map (by l'1)
        let completePath path = [s' | Join (ModDir n) <- localContext^??atMs (subPath w path)
                                    , (s',_) <- n
                                    , lst`isPrefix`s']
            completeWord l s = [s' | s' <- l, s`isPrefix`s']
            completeCommand = completeWord commandNames
            completeKeyName = completeWord (keys ks)
            completeClientKeyName = completeWord clientKeyNames
            completeBranchName k = completeWord brancheNames
              where brancheNames = keys (getVCSBranches k^.thunk)
            beginning = matches (const True) nbsp (reverse ln)
            lnWords = words ln & \l -> if beginning || empty l then l+[""] else l
        writeChan srv . CompleteResponse $!! case lnWords of
          [] -> completeCommand ""
          [cmd] -> completeCommand cmd
          ["help",cmd] -> completeCommand cmd
          ("help":_) -> []
          ["style",_,tp] -> completeWord ["color","bgcolor","display","underline","italic","bold","indent","prefix"] tp
          ["style",_,"display",tp] -> completeWord ["none","line","block","inline"] tp
          ["style",_,x,tp] | x`elem`["underline","bold","italic"] -> completeWord ["none","true","false"] tp
                           | x`elem`["color","bgcolor"] -> completeWord ("none":keys colorNames) tp
          ["key",cmd] -> completeWord ["access","list","gen","del","set","meta","grant","export","import"] cmd
          ["key","grant",acc] -> completeWord (map show [Deny .. maxBound]) acc
          ["key","grant",_,k] -> completeKeyName k
          ["key","export",k] -> completeKeyName k
          ["key","export",_,t] -> completeWord ["proof"] t
          ["key","import",k] -> completeKeyName k
          ["key","import",_,k] -> completeClientKeyName k
          ["key","set",k] -> completeKeyName k
          ["key","meta",k] -> completeKeyName k
          ["key","gen",t] -> completeWord ["client","server"] t
          ["key","del",k] -> completeClientKeyName k + completeKeyName k + completeWord ["client","server"] k
          ["key","del","client",k] -> completeClientKeyName k
          ["key","del","server",k] -> completeKeyName k
          ("key":_) -> []
          ["vcs",c] -> completeWord ["list","get","commit","checkout","branch"] c
          ["vcs","list",k] -> completeKeyName k
          ["vcs","list",k,b] -> completeBranchName k b
          ["vcs","get",x] -> completeWord ["source","library"] x
          ["vcs","branch",b] -> completeBranchName curlyPublisher b
          ["vcs","branch",_,c] -> completeWord ["keep","drop","fork","link"] c
          ["vcs","branch",_,c,u] | c`elem`["fork","link"] -> completeKeyName u
          ["vcs","branch",_,c,u,b] | c`elem`["fork","link"] -> completeBranchName u b
          ["vcs","commit",b] -> completeBranchName curlyPublisher b
          ("vcs":"commit":_:p) -> completePath (init p)
          ("vcs":_) -> []
          ["configure",p] -> completeWord [show n+":"+s | (n,s) <- curlyFiles ?curlyConfig^.ascList] p
          ["repository",cmd] -> completeWord ["list","add","contents","browse"] cmd
          ("repository":_) -> []
          ["compareTypes",x] -> completeWord ["shape","constraints"] x
          (_:t) -> completePath (init t)
        return True
      EndOfTransmission -> return False
      BannerRequest b -> True <$ do
        when b (serve conn (CommandOutput (stringBytes (getConf confBanner))))
        putMVar start ()
      _ -> return True
  let getK n = do
        clt <- dupChan (connClient conn)
        serve conn (PubkeyRequest n)
        until $ do
          x <- readChan clt
          return $ case x of
            PubkeyResponse r -> Just r
            _ -> Nothing
      keyList | hasLocalClient = return []
              | otherwise = getClientKeys
      genK b n = serve conn (KeyGenRequest b n)
      doEdit ext (l,c) b = do
        clt <- dupChan (connClient conn)
        serve conn (EditRequest ext (l,c) b)
        b' <- until $ do
          x <- readChan clt
          return $ case x of
            EditResponse r -> Just r
            _ -> Nothing
        return (b' <$ guard (b/=b'))
  let ?serve = serve conn . CommandOutput
      ?quitSession = serve conn ServerHasQuit
      ?access = acc
      ?clientOps = KeyOps getK genK keyList
  let ?edit = if acc>=Write then doEdit else \_ _ _ -> do
        serveStrLn "Error: Unauthorized write request"
        return Nothing
      ?killServer = if acc>=Admin then case getConf confThreads of
        Nothing -> serveStrLn "No server to kill"
        Just (tid1,tid2,subs) -> do
          serveStrLn (format "Killing server '%s'" (getConf confInstance))
          (_,subm) <- readIORef subs
          traverse_ killThread (tid1:tid2:toList subm)
                    else unit
  let ?subSession = fix $ \sub conf ->
        let ?subSession = sub
            ?quitSession = unit :: IO ()
            ?curlyConfig = conf
        in withCurlyPlex conf $ do
          oldPlex <- liftIO $ runAtomic compPlex (id `swapWith` const ?curlyPlex)
          interactiveSession (serve conn CommandAck)
          runStreamState (modify (cons (OC_Char '\n')))
          liftIO $ writeIORef compPlex oldPlex
      
  t <- forkIO $ do
    takeMVar start
    str <- withPrelude <$> connGetContents conn
    trylog (serve conn ServerHasQuit) $ void $ parseCurly str (interactiveSession (serve conn CommandAck))
  thr t

localClient :: String -> Connection -> IO ()
localClient user conn@(Connection clt srv) = do
  isT <- and <$> traverse hIsTerminalDevice [stdin,stdout]
  if not isT then startTerm
    else do
    trylog unit $ do
      x <- readString curlyHistoryFile
      traverse_ addHistory (lines x)
    setCompleterWordBreakCharacters " \t\n"
    setCompletionEntryFunction $ Just $ \s -> getLineBuffer >>= \ln -> do
      writeChan clt (CompleteRequest s ln)
      until $ do
        x <- readChan srv
        case x of
          CompleteResponse y -> return (Just y)
          _ -> Nothing <$ commonServerRequest clt x

    prompt <- newIORef (user+"> ")
    commonClient conn True (writeIORef prompt (user+"> ")) $ do
      p <- runAtomic prompt $ id`swapWith`const (take (length user-3) (repeat ' ')+"...> ")
      l <- readline p
      l <$ case l of
        Just s -> do
          addHistory s
          appendString curlyHistoryFile (s+"\n")
        Nothing -> unit
    let goodbye = "Have a nice day !"
    putStrLn $ "\r"+take (max (length goodbye) (1+length user)) (goodbye+repeat ' ')

  where startTerm = do
          p <- getProgName
          as <- getArgs
          let findTerminal [] = liftIOLog $ error "Couldn't find a terminal"
              findTerminal (t:ts) = do
                isRunnable <- by (otherPerms.executePerm) <$> getPermissions t
                logLine Verbose $ format "Trying terminal %s: %s" t (if isRunnable then "runnable" else "not runnable")
                if isRunnable then do
                  spawnProcess t ("-e":p:as)
                  return ()
                  else findTerminal ts
          findTerminal (liftA2 (flip (</>)) terminals paths)
          exitSuccess
        paths = fromMaybe [] $ matches Just (sepBy' (many1' (noneOf ":")) (single ':')) (envVar "" "PATH")
        terminals = ["x-terminal-emulator","lxterminal","xfce4-terminal","gnome-terminal","konsole","xterm","urxvt","rxvt"]

yesOrNo p = until $ do
  ans <- readline (p+"[y/N] ")
  matchesT Just (keyword True "y" + keyword False "n" + fill False eoi) (fold ans)

commonServerRequest clt (EditRequest ext (l,c) b) = writeChan clt . EditResponse =<< localEdit ext (l,c) b
commonServerRequest clt (PubkeyRequest name) = writeChan clt . PubkeyResponse =<< map (by l'2) . lookup name <$> getKeyStore
commonServerRequest _ (CommandOutput out) = liftIOLog (writeHBytes stdout out) 
commonServerRequest _ (KeyGenRequest True str) = do
  priv <- genPrivateKey
  let pub = publicKey priv
  modifyKeyStore $ insert str (fingerprint pub,pub,Just priv,zero,zero)
commonServerRequest _ (KeyGenRequest False str) = do
  doit <- yesOrNo $ format "Do you really want to delete the key '%s' ?" str
  when doit (modifyKeyStore $ delete str)
commonServerRequest clt KeyListRequest = do
  m <- getKeyStore
  writeChan clt $ KeyListResponse [(name,fp,has t'Just priv) | (name,(fp,_,priv,_,_)) <- m^.ascList]
commonServerRequest _ _ = unit

commonClient :: Connection -> Bool -> IO () -> IO (Maybe String) -> IO ()
commonClient (Connection clt srv) bann ack prompt = do
  writeChan clt (BannerRequest bann)
  while $ readChan srv >>= \x -> case x of
    LineRequest -> prompt >>= \l -> nonempty l <$ do
      writeChan clt (LineResponse (foldMap (+"\n") l))
    CommandAck -> True <$ ack
    ServerHasQuit -> return False
    _ -> True <$ commonServerRequest clt x
  writeChan clt EndOfTransmission

stringClient :: String -> Connection -> IO ()
stringClient str conn = do
  ln <- newIORef (Just str)
  commonClient conn False unit (runAtomic ln $ id`swapWith`const Nothing)

fileClient :: String -> Connection -> IO ()
fileClient f conn = do
  hSetBuffering stdout NoBuffering
  lns <- newChan
  forkIO $ do
    str <- case f of
      "-" -> readHString stdin
      _ -> readString f
    for_ (lines str) $ \ln -> writeChan lns (Just ln)
    writeChan lns Nothing
  commonClient conn False unit (readChan lns)

forkMVar :: IO () -> IO (MVar ())
forkMVar m = newEmptyMVar <*= \v -> forkIO $ trylog unit m >> putMVar v ()

connectChans :: forall i o m. (Serializable i,Format o,MonadIO m, ?write :: Bytes -> IO ()) => Maybe SharedSecret -> Chan i -> Chan o -> ParserT Bytes m (MVar (),MVar ())
connectChans sec i o = do
  let
    rcvX :: ParserT Bytes IO o
    sendX :: i -> IO ()
    (rcvX,sendX) = case sec of
        Just s -> let ?secret = s in (decrypt,encrypt >=> send)
        Nothing -> (receive,send)
  b <- remaining
  liftIO $ do
    sem <- forkMVar $ forever $ readChan i >>= sendX
    sem' <- forkMVar $ matchesT (const ()) (forever $ rcvX >>= \d -> liftIO (writeChan o d)) b
    return (sem,sem')

untilE :: Monad m => a -> (a -> m (a:+:b)) -> m b
untilE a k = fix (\f a -> k a >>= (f<|>return)) a

serverSecret :: (MonadIO m, ?write :: Bytes -> IO ()) => Map KeyFingerprint (String,PrivateKey) -> ParserT Bytes m (String,SharedSecret)
serverSecret ks = do
  (nm,priv) <- untilE maxBound $ \(acc,k) -> case nearest (False,GT) k ks of
    Nothing -> do
      logLine Verbose $ format "Trying key %k" (minBound :: KeyFingerprint)
      send (False,minBound :: KeyFingerprint)
      if acc<=Read then zero
        else map Left receive
    Just (k',x) | k == k' -> send (True,k') >> return (Right x)
                | otherwise -> do
                    logLine Verbose $ format "Trying key %k" k'
                    send (False,k') >> map Left receive
  logLine Verbose $ "Agreed on authorized key "+nm
  (user,pub) <- receive
  (user,) <$> sharedSecret False priv pub
clientSecret :: (MonadIO m, ?write :: Bytes -> IO ()) => String -> Map (Access,KeyFingerprint) (String,PublicKey) -> ParserT Bytes m (Access,SharedSecret)
clientSecret inst ks = do
  (acc,(nm,pub)) <- untilE maxBound $ \acc -> receive >>= \(eq,k) -> case nearest (False,GT) (acc,k) ks of
    Nothing -> do
      send (minBound :: (Access,KeyFingerprint))
      zero
    Just ((acc',k'),x) -> (if eq then Right (acc',x) else Left acc') <$ do
      logLine Verbose $ format "Sending key/access pair (%k,%s)" k' (show acc')
      unless eq (send (acc',k'))
  logLine Verbose $ "Agreed on authorized key "+nm
  priv <- genPrivateKey
  send (format "%s@%s" nm inst :: String,publicKey priv)
  (acc,) <$> sharedSecret True priv pub

handleServer :: Handle -> Connection -> IO String
handleServer  h (Connection clt srv) = do
  inst <- runConnection Just False h $ do
    identities <- getKeyStore
    let idKeys = fromAList [(f,(name,priv)) | (name,(f,_,Just priv,_,_)) <- identities^.ascList]
    authRequired <- receive
    (inst,sec) <- if authRequired then second Just<$>serverSecret idKeys
                  else logLine Verbose "No authorization required by server" >> map (,Nothing) receive
    inst <$ connectChans sec clt srv
  maybe (error "Not authorized to access server") return inst
handleClient :: String -> Handle -> Connection -> IO (Access,MVar ())
handleClient inst h (Connection clt srv) = do
  identities <- getKeyStore
  let allowedKeys = fromAList [((access,f),(name,pub))
                              | (name,(f,pub,_,_,all)) <- identities^.ascList
                              , access <- [all^.at inst.folded]
                              , access >= Read]
  clt <- runConnection Just False h $ do
    (acc,sec) <- if empty allowedKeys then send False >> send inst >> return (Read,Nothing)
                 else send True >> second Just<$>clientSecret inst allowedKeys
    (sem,sem') <- connectChans sec srv clt
    (acc,) <$> liftIO (forkMVar $ takeMVar sem >> takeMVar sem')
  maybe (error "Unauthorized client") return clt

targetServer :: (?targetParams :: TargetParams) => IO ServerEndPoint
targetServer = case getConf confServer of
  Just (inst,srv,p) -> SocketServer <$> do
    h <- connectTo srv p
    h' <- runConnection Just True h $ do
      x <- exchange (AskInstance inst)
      case x of
        Left err -> liftIO (putStrLn err) >> zero
        Right p' -> liftIO (connectTo srv p')
    maybe (error $ "Cannot connect to instance "+inst+"@"+srv+":"+show p) return h'
  Nothing -> return LocalServer


