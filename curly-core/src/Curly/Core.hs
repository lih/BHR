{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, StandaloneDeriving, PatternSynonyms, ViewPatterns, TypeFamilies #-}
module Curly.Core(
  -- * Expressions
  ExprNode(..),Expression,
  Identifier(..),HasIdents(..),Builtin(..),BinaryRelocation(..),
  SemanticT(..),Semantic(..),mkAbstract,mkSymbol,mkApply,sem,
  Hash(..),hashData,LibraryID(..),GlobalID(..),
  pattern PatSymbol,pattern PatAbstract,pattern PatApply,pattern PatApply2,
  t'Symbol,t'Apply,t'Abstract,
  -- ** Utilities
  c'Expression,syntax,semantic,mapParams,
  -- * Environment
  envVar,curlyDataRoot,curlyConfigRoot,curlyCacheRoot,
  curlyKeysFile,curlyCacheDir,curlyCommitDir,curlyPort,
  -- * Logging facilities
  LogLevel(..),LogMessage(..),serialWriteHBytes,addLogCallback,removeLogCallback,withLogCallback,envLogLevel,logLine,logMessage,logAction,trylogLevel,trylog,liftIOLog,cyDebug,
  -- * Misc
  B64Chunk(..),PortNumber,watchFile,connectTo,(*+),cacheFileName,createFileDirectory,
  Compressed(..),noCurlySuf,(</>),format,
  ) where

import Codec.Compression.Zlib (compress,decompress)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO,MVar,newEmptyMVar,putMVar,readMVar)
import Control.DeepSeq
import Control.Exception (bracket)
import Curly.Core.Documentation
import Data.IORef
import Definitive
import IO.Filesystem ((</>),takeFileName,dropFileName)
import IO.Network.Socket (PortNumber,connect,getAddrInfo)
import Language.Format
import qualified Curly.Core.Security.SHA256 as SHA256
import qualified Data.ByteString.Base64 as Base64
import qualified System.FSNotify as FSNotify
import System.Directory (createDirectoryIfMissing,doesDirectoryExist)
import System.Environment (lookupEnv)
import System.Environment.XDG.BaseDir
import System.IO (openFile,IOMode(AppendMode),hSetBuffering,BufferMode(LineBuffering))

{-| The type of an expression node

This type is used in combination with others within Free functors to
model expressions with different attributes.
-}
data ExprNode s a = Apply a a
                  | Lambda s a
                  deriving (Eq,Ord,Show,Generic)
instance (NFData s,NFData a) => NFData (ExprNode s a) where
  rnf (Apply x y) = rnf x`seq`rnf y`seq`()
  rnf (Lambda s e) = rnf s`seq`rnf e`seq`()

-- | The type of a simple Curly expression
type Expression s a = Free (ExprNode s) a
instance Functor (ExprNode s) where
  map f (Apply a b) = Apply (f a) (f b)
  map f (Lambda s a) = Lambda s (f a)
instance Foldable (ExprNode s) where
  fold (Lambda _ a) = a
  fold (Apply a b) = a+b
instance Traversable (ExprNode s) where
  sequence (Lambda s a) = Lambda s<$>a
  sequence (Apply ff fx) = Apply<$>ff<*>fx
instance (Serializable Bytes a,Serializable Bytes s) => Serializable Bytes (ExprNode s a)
instance (Serializable Bytes a,Serializable Bytes s) => Serializable Bytes (Free (ExprNode s) a) where encode = encodeFree
instance (Format Bytes a,Format Bytes s) => Format Bytes (ExprNode s a)
instance (Format Bytes a,Format Bytes s) => Format Bytes (Free (ExprNode s) a) where datum = datumFree

c'Expression :: Constraint (Expression a b)
c'Expression = c'_

data SemanticT e i o = SemApply e e
                     | SemAbstract i e
                     | SemSymbol o
{- | The class of all lambda-like expressions.

This class provides an abstraction of the different types used to
represent expressions at the different stages of compilation.

This class provides three constructors and a destructor for its
target type, allowing abstract pattern-matching to take place.
-}
class Semantic e i o | e -> i o where
  semNode :: Iso' e (SemanticT e i o)
instance Semantic (Free (ExprNode s) a) s a where
  semNode = iso f g
    where f (Pure s) = SemSymbol s
          f (Join (Lambda s e)) = SemAbstract s e
          f (Join (Apply a b)) = SemApply a b
          g (SemSymbol s) = Pure s
          g (SemAbstract s e) = Join (Lambda s e)
          g (SemApply a b) = Join (Apply a b)

sem :: Semantic e i o => e -> SemanticT e i o
sem = by semNode

mkSymbol :: Semantic e i o => o -> e
mkSymbol x = SemSymbol x^..semNode

mkAbstract :: Semantic e i o => i -> e -> e
mkAbstract s e = SemAbstract s e^..semNode

mkApply :: Semantic e i o => e -> e -> e
mkApply a b = SemApply a b^..semNode

pattern PatSymbol :: Semantic e i o => o -> e
pattern PatSymbol s <- (sem -> SemSymbol s)

pattern PatAbstract :: Semantic e i o => i -> e -> e
pattern PatAbstract s e <- (sem -> SemAbstract s e)

pattern PatApply :: Semantic e i o => e -> e -> e
pattern PatApply f x <- (sem -> SemApply f x)
pattern PatApply2 :: Semantic e i o => e -> e -> e -> e
pattern PatApply2 f x y <- PatApply (PatApply f x) y

-- | Transform a lambda-like expression into another
semantic :: (Semantic e i o, Semantic e' i o) => e -> e'
semantic e = case sem e of
  SemSymbol s -> mkSymbol s
  SemAbstract i e' -> mkAbstract i (semantic e')
  SemApply f x -> mkApply (semantic f) (semantic x)

t'Symbol :: Semantic e i o => Traversal' e o
t'Symbol k (sem -> SemSymbol s) = mkSymbol <$> k s
t'Symbol _ x = pure x
t'Apply :: Semantic e i o => Traversal' e (e,e)
t'Apply k (sem -> SemApply f x) = uncurry mkApply<$>k (f,x)
t'Apply _ x = pure x
t'Abstract :: Semantic e i o => Traversal' e (i,e)
t'Abstract k (sem -> SemAbstract s e) = uncurry mkAbstract<$>k (s,e)
t'Abstract _ x = pure x

-- | Tranform an expression into another, annotating it with contextual information.
{-# INLINE syntax #-}
syntax :: (Semantic e i o,Semantic e' i o'',Ord i) => (o -> o' -> o'') -> (o -> o') -> (o -> i) -> (Int -> o') -> e -> e'
syntax mergeSym val name loc = syn (zero :: Int,c'map zero)
  where syn (depth,syms) = fix $ \syn' e -> case sem e of
          SemSymbol o -> mkSymbol $ mergeSym o $ maybe (val o) (loc . \depth' -> (depth-depth')-1) (syms^.at (name o))
          SemAbstract i e' -> mkAbstract i (syn (depth+1,insert i depth syms) e')
          SemApply f x -> mkApply (syn' f) (syn' x)

-- | Maps a function over lambda parameters in an expression
mapParams :: (Semantic e i o,Semantic e' i' o) => (i -> i') -> e -> e'
mapParams f = doMap
  where doMap x = case sem x of
          SemSymbol s -> mkSymbol s
          SemAbstract s e -> mkAbstract (f s) (doMap e)
          SemApply a b -> mkApply (doMap a) (doMap b)

instance (Documented s,Documented a) => Documented (Expression s a) where
  document expr = docTag' "expr" [Pure $ show' "" expr]
    where
      show' :: forall a' s'. (Documented a',Documented s') => String -> Expression s' a' -> String
      show' h (Pure s) = h+"-> "+pretty s
      show' h (Join (Lambda s e@(Join (Lambda _ _)))) = h+"<- "+pretty s+" "+drop (length h+3) (show' h e)
      show' h (Join (Lambda s e)) = h+"<- "+pretty s+"\n"+show' (h+"| ") e
      show' h (Join (Apply (Join (Apply f (Pure x1))) (Pure x2)))
        = show' h (Join (Apply (map pretty f) (Pure (pretty x1+" "+pretty x2))))
      show' h (Join (Apply (Pure f) (Pure x))) = h+"-> "+pretty f+"("+pretty x+")"
      show' h (Join (Apply f x)) = show' h f+"\n"
                                       +show' (h+"- ") x

newtype B64Chunk = B64Chunk Chunk
instance Show B64Chunk where
  show (B64Chunk l) = foldMap to $ Base64.encode l^.i'elems
    where to '/' = "-"
          to '+' = "_"
          to '=' = []
          to x = [x]
instance Read B64Chunk where
  readsPrec _ = readsParser $ do
    let tr '-' = '/'
        tr '_' = '+'
        tr x = x
        pad c = c+take (negate (length c)`mod`4) "===="
    c <- many' (tr <$> satisfy p)
    (const zero <|> return . B64Chunk) (Base64.decode (pad c^..i'elems))
    where p x = inRange 'a' 'z' x || inRange 'A' 'Z' x || inRange '0' '9' x || x=='_' || x=='-'

-- | `envVar def var` retrieves a `var` from the environment, or returns `def` if the former doesn't exist
envVar :: String -> String -> String
envVar d s = fromMaybe d (lookupEnv s^.thunk)

curlyDirPath :: FilePath -> FilePath
curlyDirPath dir = (createDirectoryIfMissing True dir^.thunk)`seq`dir

-- | The default Curly port for library proxies and the portmapper
curlyPort :: PortNumber
curlyPort = fromMaybe 25465 $ matches Just number (envVar "" "CURLY_PORT")

curlyDataRoot :: FilePath
curlyDataRoot = curlyDirPath $ getUserDataDir "curly"^.thunk
curlyConfigRoot :: FilePath
curlyConfigRoot = curlyDirPath $ getUserConfigDir "curly"^.thunk
curlyCacheRoot :: FilePath
curlyCacheRoot = curlyDirPath $ getUserCacheDir "curly"^.thunk

-- | The path of the Curly key wallet
curlyKeysFile :: FilePath
curlyKeysFile = curlyDataRoot + "/keys"

-- | The path to the user's cache directory
curlyCacheDir :: FilePath
curlyCacheDir = curlyDirPath $ envVar (curlyCacheRoot + "/libraries") "CURLY_LIBCACHE"

curlyCommitDir :: FilePath
curlyCommitDir = curlyDirPath (curlyCacheRoot + "/commits")

-- | A Curly log level
data LogLevel = Quiet | Chatty | Verbose | Debug
              deriving (Eq,Ord,Show,Generic)
instance Serializable Bytes LogLevel
instance Format Bytes LogLevel
data LogMessage = LogLine LogLevel String
                | LogActionStart String
                | LogActionEnd String Bool
                deriving (Show,Generic)
instance Format Bytes LogMessage
instance Serializable Bytes LogMessage

-- The global log level, as set by the environment variable CURLY_LOGLEVEL
envLogLevel :: LogLevel
envLogLevel = envVar "chatty" "CURLY_LOGLEVEL"
              & fromMaybe Chatty . matches Just (foldl1' (<+?) [x<$several s | (x,s) <- levels])
  where levels = [(Quiet,"quiet"),(Chatty,"chatty"),(Verbose,"verbose"),(Debug,"debug")]

-- | Logs a line to stderr if the environment log level is greater than the given threshold
logLine :: MonadIO m => LogLevel -> String -> m ()
logLine level str = logMessage (LogLine level str)

logMessage :: MonadIO m => LogMessage -> m ()
logMessage msg = initLogChannel`seq`liftIO $ writeChan logChannel msg

logAction :: MonadIO m => String -> IO a -> m a
logAction act ma = do
  logMessage (LogActionStart act)
  a <- liftIO $ catch (\e -> logMessage (LogActionEnd act False) >> throw e) ma
  logMessage (LogActionEnd act True)
  return a

newtype LogCallbackID = LogCallbackID Int
                      deriving (Eq,Ord,Show)
logCallbacks :: IORef ([LogCallbackID],Map LogCallbackID (LogMessage -> IO ()))
logCallbacks = by thunk $ do
  inAction <- newIORef (False,"")
  let def (LogLine level str) | envLogLevel>=level = do
        (b,ind) <- readIORef inAction
        when b (writeLog "\n")
        modifyIORef inAction (set l'1 False)
        writeLog (ind+str+"\n")
                              | otherwise = unit
      def (LogActionStart act) = do
        (b,ind) <- readIORef inAction
        when b (writeLog "\n")
        modifyIORef inAction (set l'1 True . second ("  "+))
        writeLog (ind+"Begin "+act+"...")
      def (LogActionEnd act success) = do
        modifyIORef inAction (second (drop 2))
        readIORef inAction >>= \(b,ind) ->
          if b
          then writeLog (if success then " done.\n" else " failed.\n")
          else writeLog (ind + (if success then "Finished " else "Failed ") + act + "\n")
        modifyIORef inAction (set l'1 False)
      writeLog = serialWriteHBytes logFile . stringBytes
  newIORef (map LogCallbackID [1..],singleton (LogCallbackID 0) (logFile`seq`def))
                                              
logChannel :: Chan LogMessage
logChannel = newChan^.thunk

lineChannel :: Chan (Handle,Bytes,MVar ())
lineChannel = by thunk $ do
  c <- newChan
  _ <- forkIO $ forever $ do
    (h,bs,v) <- readChan c
    try unit (writeHBytes h bs)
    putMVar v ()
  return c
  
serialWriteHBytes :: Handle -> Bytes -> IO ()
serialWriteHBytes h bs = do
   v <- newEmptyMVar
   writeChan lineChannel (h,bs,v)
   readMVar v

initLogChannel :: ()
initLogChannel = by thunk $ void $ forkIO $ forever $ do
  msg <- readChan logChannel
  (_,cbs) <- readIORef logCallbacks
  for_ cbs $ ($msg)

addLogCallback :: (LogMessage -> IO ()) -> IO LogCallbackID
addLogCallback c = runAtomic logCallbacks $ do id <~ \((i:is),m) -> ((is,insert i c m),i)
removeLogCallback :: LogCallbackID -> IO ()
removeLogCallback i = modifyIORef logCallbacks $ (i:) <#> delete i

withLogCallback :: (LogMessage -> IO ()) -> IO a -> IO a
withLogCallback cb ma = bracket (addLogCallback cb) removeLogCallback (const ma)  

cyDebug :: Show a => a -> a
cyDebug | envLogLevel >= Debug = debug
        | otherwise = id

-- | A global handle to a log file (avoids reopening the same file over and over again)
logFile :: Handle
logFile = case envVar "" "CURLY_LOGFILE" of
  "" -> stderr
  f -> (openFile f AppendMode <*= \h -> hSetBuffering h LineBuffering)^.thunk


-- | Runs an IO action, logging its errors if the given log level is lower than the environment
trylogLevel :: LogLevel -> IO a -> IO a -> IO a
trylogLevel l def = catch (\e -> logLine l (show e) >> def)
-- | Same as `tryLogLevel`, with a log level of `Debug`
trylog :: IO a -> IO a -> IO a
trylog = trylogLevel Debug
-- | A utility function that lifts its argument while logging its errors
liftIOLog :: MonadIO m => IO () -> m ()
liftIOLog = liftIO . trylogLevel Quiet unit

-- | A global INotify instance
inotify :: FSNotify.WatchManager
inotify = FSNotify.startManager^.thunk
-- | Sets a watch on the given file, on the usual signals
watchFile :: FilePath -> IO () -> IO (IO ())
watchFile s f = do
  isD <- doesDirectoryExist s
  if isD then
    FSNotify.watchTree inotify s (const True) (\_ -> f)
    else FSNotify.watchTree inotify (dropFileName s) (\p -> takeFileName (FSNotify.eventPath p) == takeFileName s) (\_ -> f)

-- | A utility function that opens a client socket to the given server and port
connectTo :: String -> PortNumber -> IO Handle
connectTo h p = trylog (error $ format "Couldn't connect to host %s:%p" h p) $ do
  connect . head =<< getAddrInfo Nothing (Just h) (Just (show p))

-- | Inclusive-or for `Map`s
(*+) :: (Ord k,Semigroup m) => Map k m -> Map k m -> Map k m
a *+ b = a*b+a+b

cacheFileName :: FilePath     -- ^ A base directory
                 -> String    -- ^ A file name
                 -> String    -- ^ An extension
                 -> FilePath
cacheFileName base (c0:c1:cs@(_:_)) ext = base</>[c0,c1]</>cs+"."+ext
cacheFileName base x ext = base</>x+"."+ext
createFileDirectory :: FilePath -> IO ()
createFileDirectory p = createDirectoryIfMissing True (dropFileName p)

{- | The class of Curly identifiers, used mainly to simplify type signatures. -}
class (Ord s,Show s,NFData s) => Identifier s where
  pureIdent :: String -> s
  identName :: s -> String
instance Identifier String where pureIdent = id; identName = id
instance Identifier Int where
  pureIdent = error "Cannot construct numeric identifier from arbitrary string"
  identName = show

-- | A useful class for identifier-filled types
class HasIdents s s' t t' | t t' -> s s' where
  ff'idents :: FixFold s s' t t'
instance (Traversable f,HasIdents s s' (f (Free f' a)) (f' (Free f' a))) => HasIdents s s' (Free f a) (Free f' a) where
  ff'idents k = f
    where f (Pure a) = pure (Pure a)
          f (Join ffa) = map Join (traverse f ffa >>= traversel ff'idents k)
instance forall s s' g g' f f' a. (Traversable f,HasIdents s s' (g a) (g' a), HasIdents s s' (f (g' a)) (f' (g' a))) => HasIdents s s' ((f:.:g) a) ((f':.:g') a) where
  ff'idents k (Compose x) = Compose<$>(traversel (traverse.ff'idents) k x >>= \y -> traversel ff'idents k (y :: f (g' a)))
instance HasIdents s s' (ExprNode s a) (ExprNode s' a) where
  ff'idents k (Lambda s a) = k s <&> \s' -> Lambda s' a
  ff'idents _ (Apply x y) = pure (Apply x y)
instance HasIdents s s' (s,a) (s',a) where
  ff'idents k (s,a) = map (,a) (k s)
instance HasIdents s s' t t' => HasIdents s s' (Maybe t) (Maybe t') where
  ff'idents = t'Just.ff'idents

data RelocationSize = RS_16 | RS_32 | RS_64
                    deriving (Eq,Ord,Show,Generic)
instance Serializable Bytes RelocationSize ; instance Format Bytes RelocationSize
data BinaryRelocation = BinaryRelocation {
  _br_PCRelative :: Bool,
  _br_size :: RelocationSize,
  _br_symhash :: Hash,
  _br_symoffset :: Int
  }
                      deriving (Eq,Ord,Show,Generic)
instance Serializable Bytes BinaryRelocation
instance Format Bytes BinaryRelocation
-- | The type of all Curly builtins
data Builtin = B_Undefined
             | B_Seq
             | B_Unit 

             | B_Number Int
             | B_AddInt | B_SubInt | B_MulInt | B_DivInt
             | B_CmpInt_LT | B_CmpInt_EQ
               
             | B_String String
             | B_StringLength
             | B_AddString | B_ShowInt

             | B_Bytes Bytes

             | B_MkArray
             | B_ArrayLength
             | B_ArrayAt
             | B_ArraySet

             | B_SyntaxNode
             | B_SyntaxSym
             | B_SyntaxExpr
             | B_SyntaxInd

             | B_ExprLambda
             | B_ExprApply
             | B_ExprSym
             | B_ExprInd

             | B_FileDesc Int
             | B_Open | B_Read | B_Write | B_Close

             | B_Foreign (Map String GlobalID) GlobalID

             | B_Relocatable Bool Hash [(Bytes,BinaryRelocation)] Bytes
             | B_RawIndex Int
             deriving (Eq,Ord,Show,Generic)
instance Documented Builtin where
  document = Pure . show'
    where show' (B_Number n) = show n
          show' (B_String s) = show s
          show' b = show b
instance Serializable Bytes Builtin where
instance Format Bytes Builtin where
instance NFData Builtin where rnf b = b`seq`()

newtype Compressed a = Compressed { unCompressed :: a }
                     deriving (Show,Eq,Ord)
instance Serializable Bytes a => Serializable Bytes (Compressed a) where
  encode p (Compressed a) = encode p (compress (serialize a))
instance Format Bytes a => Format Bytes (Compressed a) where
  datum = (datum <&> decompress) >*> (Compressed <$> datum)

noCurlySuf :: FilePath -> Maybe FilePath
noCurlySuf f = nosuffix ".cy" f + nosuffix ".curly" f + nosuffix ".cyl" f
  where nosuffix s s' = if t==s then Just h else Nothing
          where (h,t) = splitAt (length s'-length s) s'

newtype Hash = Hash Chunk
             deriving (Eq,Ord)
hashData :: Bytes -> Hash
hashData b = Hash (SHA256.hashlazy b)
instance Show Hash where
  show (Hash h) = show (B64Chunk h)
instance Read Hash where
  readsPrec _ = readsParser (readable <&> \(B64Chunk h) -> Hash h)
instance Serializable Bytes Hash where
  encode _ (Hash h) = h^.chunkBuilder
instance Format Bytes Hash where
  datum = Hash<$>getChunk 32

newtype LibraryID = LibraryID Chunk
                deriving (Eq,Ord,Generic)
idSize :: Int
idSize = 32
instance Serializable Bytes LibraryID where
  encode _ (LibraryID x) = x^.chunkBuilder
instance Format Bytes LibraryID where
  datum = LibraryID<$>getChunk idSize
instance NFData LibraryID
instance Show LibraryID where
  show (LibraryID l) = show (B64Chunk l)
instance Read LibraryID where
  readsPrec _ = readsParser (readable >>= \(B64Chunk c) -> LibraryID c <$ guard (chunkSize c==idSize))
instance Documented LibraryID where document l = Pure (show l)

data GlobalID = GlobalID String (Maybe (String,LibraryID))
           deriving (Eq,Ord,Show,Generic)
instance Documented GlobalID where
  document = if envLogLevel>=Verbose
             then \(GlobalID n l) -> Pure (n+showL l)
             else \(GlobalID n _) -> Pure n
    where showL (Just (n,l)) = "["+show l+":"+n+"]"
          showL _ = "[]"
instance Serializable Bytes GlobalID
instance Format Bytes GlobalID
instance NFData GlobalID
instance Identifier GlobalID where
  pureIdent n = GlobalID n Nothing
  identName (GlobalID n _) = n
