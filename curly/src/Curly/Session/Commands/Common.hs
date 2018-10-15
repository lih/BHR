{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo, NoMonomorphismRestriction #-}
module Curly.Session.Commands.Common where

import Curly.Core
import Curly.Core.Library
import Curly.Core.Security
import Curly.UI
import Curly.UI.Options hiding (nbsp,spc)
import Curly.Core.Parser
import Curly.Style
import Curly.Core.Documentation
import Data.IORef 
import Language.Format hiding (space)
import Control.Exception (fromException)
import Control.DeepSeq (($!!))
import System.Process (readProcess)
import Control.Concurrent.MVar
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Prelude as P

q_string :: TH.QuasiQuoter
q_string = TH.QuasiQuoter {
  TH.quoteExp = \s -> P.return (TH.LitE (TH.StringL ("{section.help-doc "+s+"}"))),
  TH.quotePat = undefined,
  TH.quoteType = undefined,
  TH.quoteDec = undefined
  }

showPath l = intercalate "." (map (foldMap quote) l)
  where quote '.' = "\\."
        quote '\\' = "\\\\"
        quote c = [c]
subPath :: [String] -> [String] -> [String]
subPath = foldl' enter
  where enter [] ":" = []
        enter l ":" = init l
        enter p s = p+[s]

showMetaDir (Pure x) = x
showMetaDir (Join (ModDir m)) = intercalate "\n" [showSM s a | (s,a) <- m]
  where showSM s x@(Join (ModDir _)) = format "* %s:\n%s" s (indent "  " (showMetaDir x))
        showSM s (Pure y) = format "- %s: %s" s y
        indent i s = unlines (map (i+) (lines s))

optimized e = lift (getl l'library) <&> \l -> optExprIn l e

liftIOWarn :: (?sessionState :: IORef SessionState,?serve :: Bytes -> IO (),MonadIO m) => IO () -> m ()
liftIOWarn ma = liftIO $ (`catch`ma) $ \e -> case fromException e of
  Just pe@(CurlyParserException s ws) -> do
    runAtomic ?sessionState $ do warnings =- (s,ws)
    ?serve (stringBytes $ pretty pe+"\n")
  Nothing -> ?serve (stringBytes $ show e+"\n")

toTerminal :: String -> String
toTerminal | envVar "xterm" "TERM" == "screen" = toASCII
           | otherwise = id
  where toASCII = foldMap tr
        tr '⇒' = "=>"
        tr '→' = "->"
        tr c = [c]
serveString :: (?sessionState :: IORef SessionState,?serve :: Bytes -> IO (),MonadIO m) => String -> m ()
serveString s = liftIOWarn (?serve $!! stringBytes $ toTerminal s)
serveStrLn :: (?sessionState :: IORef SessionState,?serve :: Bytes -> IO (),MonadIO m) => String -> m ()
serveStrLn s = liftIOWarn (?serve $!! stringBytes $ toTerminal (s+"\n"))

editSource f (l,c) m = readBytes f >>= ?edit ".cy" (l,c) >>= maybe unit (\b -> writeBytes f b >> m)

data SessionState = SessionState {
  _wd :: [String],
  _style :: Style,
  _patterns :: DocPatterns,
  _this :: Library,
  _warnings :: (Maybe String,[Warning])
  }
wd :: Lens' SessionState [String]
wd = lens _wd (\x y -> x { _wd = y })
this :: Lens' SessionState Library
this = lens _this (\x y -> x { _this = y })
style :: Lens' SessionState Style
style = lens _style (\x y -> x { _style = y })
warnings :: Lens' SessionState (Maybe String,[Warning])
warnings = lens _warnings (\x y -> x { _warnings = y })
patterns :: Lens' SessionState DocPatterns
patterns = lens _patterns (\x y -> x { _patterns = y })

withSessionState :: (?curlyPlex :: CurlyPlex, MonadIO m) => ((?sessionState :: IORef SessionState) => m a) -> m a
withSessionState io = do
  istate <- liftIO $ newIORef (SessionState [] defaultStyle zero zero zero)
  let reloadContext m = runAtomic istate $ do
        is <- getl (this.imports)
        let cxt = context m
            is' = zipWith (flip const) is cxt
        this =~ set imports is' . compose [warp symbols (insert n v) | (GlobalID n _,v) <- toList is']
  liftIO $ runAtomic (?curlyPlex^.mountainCache) (l'2 =~ (reloadContext:))
  let ?sessionState = istate in io

withSessionLib :: (MonadIO m,?sessionState :: IORef SessionState) => OpParser m a -> OpParser m a
withSessionLib ma = do
  l <- by this <$> liftIO (readIORef ?sessionState)
  lift (l'library =- l)
  ma <* (lift (getl l'library) >>= liftIO . modifyIORef ?sessionState . set this)
 
withStyle :: (?sessionState :: IORef SessionState,MonadIO m) => ((?style :: Style) => m a) -> m a
withStyle m = getSession style >>= \s -> let ?style = s in m
withPatterns :: (?sessionState :: IORef SessionState,MonadIO m) => ((?patterns :: DocPatterns) => m a) -> m a
withPatterns m = getSession patterns >>= \ps -> let ?patterns = ps in m
getSession :: (?sessionState :: IORef SessionState,MonadIO m) => Lens' SessionState a -> m a
getSession l = liftIO (readIORef ?sessionState <&> by l)

data KeyInfo = KeyInfo PublicKey Metadata (Maybe PrivateKey)
instance Serializable Word8 Builder Bytes KeyInfo where
  encode p (KeyInfo x y z) = encode p (x,z,y)
instance Format Word8 Builder Bytes KeyInfo where
  datum = (\x y z -> KeyInfo x z y) <$> datum <*> datum <*> (datum <+? fill (Metadata zero) (remaining >>= guard . (==0) . bytesSize))

data KeyOps = KeyOps {
  opsGetKey :: String -> IO (Maybe KeyInfo),
  opsKeyGen :: Bool -> String -> IO (),
  opsListKeys :: IO [(String,KeyFingerprint,Bool)]
  }
clientKey ::(?clientOps :: KeyOps) => String -> IO (Maybe KeyInfo)
clientKey = opsGetKey ?clientOps
clientKeyGen ::(?clientOps :: KeyOps) => Bool -> String -> IO ()
clientKeyGen = opsKeyGen ?clientOps
clientKeyList ::(?clientOps :: KeyOps) => IO [(String,KeyFingerprint,Bool)]
clientKeyList = opsListKeys ?clientOps

type Interactive t = (?sessionState :: IORef SessionState
                     ,?targetParams :: TargetParams
                     ,?curlyPlex :: CurlyPlex
                     ,?curlyConfig :: CurlyConfig
                     ,?serve :: Bytes -> IO ()
                     ,?edit :: String -> (Int,Int) -> Bytes -> IO (Maybe Bytes)
                     ,?killServer :: IO ()
                     ,?quitSession :: IO ()
                     ,?access :: Access
                     ,?subSession :: CurlyConfig -> OpParser IO ()
                     ,?clientOps :: KeyOps
                     ,?terminal :: ANSITerm)
                     => t
type Command = (Documentation,OpParser IO Bool)

withDoc d m = (mkDoc "cmdDoc" d,m)

dirArg :: (MonadParser s m p, ParseStream c s, TokenPayload c ~ Char, Monad m) => p String
dirArg = many1' $ noneOf " \t\n(){}"
absPath :: (?sessionState :: IORef SessionState, MonadParser s m p, ParseStream c s, TokenPayload c ~ Char, Monad m, MonadIO p)
           => String -> p [String]
absPath lim = (single '.' >> symPath lim)
              <+? (liftA2 subPath (getSession wd) (symPath lim))


data CurlyDNSQuery = DomainVC (Proxy (String,PortNumber))
                   | DomainKey String (Proxy (Zesty KeyInfo))
dns_lookup :: (MonadIO m,Read a) => (Proxy a -> CurlyDNSQuery) -> m (Maybe a)
dns_lookup k = liftIO $ do
  p <- curlyDataFileName "dns-lookup.sh"
  let t = Proxy
  case k t of
    DomainVC _ -> readProcess "sh" [p,"domain-vc"] "" <&> \s -> map (response t) (matches Just readable s)
    DomainKey d _ -> readProcess "sh" [p,"domain-key",d] "" <&> \s -> map (response t) (matches Just readable s)

  where response :: Proxy a -> a -> a
        response _ x = x
