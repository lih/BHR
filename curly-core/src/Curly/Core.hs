{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, StandaloneDeriving, PatternSynonyms, ViewPatterns #-}
module Curly.Core(
  -- * Expressions
  ExprNode(..),Expression,
  Identifier(..),HasIdents(..),Builtin(..),
  SemanticT(..),Semantic(..),mkAbstract,mkSymbol,mkApply,sem,
  pattern PatSymbol,pattern PatAbstract,pattern PatApply,pattern PatApply2,
  -- ** Utilities
  c'Expression,syntax,semantic,mapParams,
  -- * Pretty-printing
  Pretty(..),pretty,indent,(</>),FormatArg(..),FormatType(..),format,
  -- * Environment
  envVar,curlyUserDir,curlyKeysFile,curlyCacheDir,curlyCommitDir,curlyPort,curlyBackendDir,
  -- * Conditional output
  LogLevel(..),envLogLevel,logLine,trylogLevel,trylog,liftIOLog,cyDebug,
  -- * Misc
  PortNumber,watchFile,connectTo,(*+)
  ) where

import Definitive
import Language.Format
import Control.DeepSeq
import IO.Filesystem ((</>))
import IO.Network.Socket (PortNumber,connect,getAddrInfo)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.INotify
import System.IO (openFile,IOMode(AppendMode),hSetBuffering,BufferMode(LineBuffering))
import qualified Data.ByteString.Base64 as Base64

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
instance (Serializable a,Serializable s) => Serializable (ExprNode s a)
instance (Format a,Format s) => Format (ExprNode s a)

instance (Serializable (f (Free f a)),Serializable a) => Serializable (Free f a) where
  encode (Pure s) = encodeAlt 0 s
  encode (Join f) = encodeAlt 1 f
instance (Format (f (Free f a)),Format a) => Format (Free f a) where
  datum = datumOf [FormatAlt Pure,FormatAlt Join]
instance (Serializable (f (Cofree f a)),Serializable a) => Serializable (Cofree f a) where
  encode (Step a fc) = encode (a,fc)
instance (Format (f (Cofree f a)),Format a) => Format (Cofree f a) where
  datum = uncurry Step<$>datum
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
mkSymbol x = SemSymbol x^..semNode
mkAbstract s e = SemAbstract s e^..semNode
mkApply a b = SemApply a b^..semNode

pattern PatSymbol s <- (sem -> SemSymbol s)
pattern PatAbstract s e <- (sem -> SemAbstract s e)
pattern PatApply f x <- (sem -> SemApply f x)
pattern PatApply2 f x y <- PatApply (PatApply f x) y

-- | Transform a lambda-like expression into another
semantic :: (Semantic e i o, Semantic e' i o) => e -> e'
semantic e = case sem e of
  SemSymbol s -> mkSymbol s
  SemAbstract i e' -> mkAbstract i (semantic e')
  SemApply f x -> mkApply (semantic f) (semantic x)

-- | Tranform an expression into another, annotating it with contextual information.
{-# INLINE syntax #-}
syntax :: (Semantic e i o,Semantic e' i o'',Ord i) => (o -> o' -> o'') -> (o -> o') -> (o -> i) -> (Int -> o') -> e -> e'
syntax cons val name loc = syn (zero :: Int,c'map zero)
  where syn (n,m) = fix $ \syn' e -> case sem e of
          SemSymbol o -> mkSymbol $ cons o $ maybe (val o) (loc . \m -> (n-m)-1) (m^.at (name o))
          SemAbstract i e' -> mkAbstract i (syn (n+1,insert i n m) e')
          SemApply f x -> mkApply (syn' f) (syn' x)

-- | Maps a function over lambda parameters in an expression
mapParams :: (Semantic e i o,Semantic e' i' o) => (i -> i') -> e -> e'
mapParams f = doMap
  where doMap x = case sem x of
          SemSymbol s -> mkSymbol s
          SemAbstract s e -> mkAbstract (f s) (doMap e)
          SemApply a b -> mkApply (doMap a) (doMap b)

-- | A type for objects that should be printed prettily
newtype Pretty a = Pretty a

-- | A shortcut for @show . Pretty@
pretty :: Show (Pretty a) => a -> String
pretty = show . Pretty

-- | Prepend the second to each line of the first.
indent :: String -> String -> String
indent p s = p+indent' s
  where indent' ('\n':t) = '\n':(p+indent' t)
        indent' [] = []
        indent' (c:t) = c:indent' t

instance Show (Pretty String) where show (Pretty s) = s
instance Show (Pretty Int) where show (Pretty n) = show n
instance (Show (Pretty s),Show (Pretty a)) => Show (Pretty (Expression s a)) where
  show (Pretty expr) = show' "" expr
    where
      show' :: forall a' s'. (Show (Pretty a'),Show (Pretty s')) => String -> Expression s' a' -> String
      show' h (Pure s) = h+"->  "+pretty s
      show' h (Join (Lambda s e@(Join (Lambda _ _)))) = h+"<-  "+pretty s+" "+drop (length h+4) (show' h e)
      show' h (Join (Lambda s e)) = h+"<-  "+pretty s+"\n"+show' (h+"| ") e
      show' h (Join (Apply (Join (Apply f (Pure x1))) (Pure x2)))
        = show' h (Join (Apply (map pretty f) (Pure (pretty x1+" "+pretty x2))))
      show' h (Join (Apply (Pure f) (Pure x))) = h+"-> "+pretty f+"("+pretty x+")"
      show' h (Join (Apply f x)) = show' h f+"\n"
                                       +show' (h+"- ") x
instance Show (Pretty Chunk) where
  show (Pretty l) = foldMap to $ Base64.encode l^.i'elems
    where to '/' = "-"
          to '+' = "_"
          to '=' = []
          to x = [x]
instance Read (Pretty Chunk) where
  readsPrec _ = readsParser $ do
    let from '-' = '/'
        from '_' = '+'
        from x = x
        pad c = c+take (negate (length c)`mod`4) "===="
    c <- many' (from <$> satisfy p)
    (const zero <|> return . Pretty) (Base64.decode (pad c^..i'elems))
    where p x = inside 'a' 'z' x || inside 'A' 'Z' x || inside '0' '9' x || x=='_' || x=='-'
-- | `envVar def var` retrieves a `var` from the environment, or returns `def` if the former doesn't exist
envVar :: String -> String -> String
envVar d s = fromMaybe d (lookupEnv s^.thunk)

curlyDirPath :: String -> String
curlyDirPath dir = (createDirectoryIfMissing True dir^.thunk)`seq`dir

-- | The default Curly port for library proxies and the portmapper
curlyPort :: PortNumber
curlyPort = fromMaybe 25465 $ matches Just number (envVar "" "CURLY_PORT")

-- | A user-writable directory to store Curly configurations
curlyUserDir :: String
curlyUserDir = curlyDirPath $ envVar "/tmp" "HOME"+"/.curly"

-- | The path of the Curly key wallet
curlyKeysFile :: String
curlyKeysFile = curlyUserDir + "/keys"

-- | The path to the user's cache directory
curlyCacheDir :: String
curlyCacheDir = curlyDirPath $ envVar (curlyUserDir + "/libraries") "CURLY_LIBCACHE"

curlyCommitDir :: String
curlyCommitDir = curlyDirPath (curlyUserDir + "/commits")

curlyBackendDir :: String
curlyBackendDir = envVar "/usr/lib/curly/backend" "CURLY_BACKENDS"

-- | A Curly log level
data LogLevel = Quiet | Verbose | Debug
              deriving (Eq,Ord)
-- The global log level, as set by the environment variable CURLY_LOGLEVEL
envLogLevel = envVar "quiet" "CURLY_LOGLEVEL"
              & fromMaybe Quiet . matches Just (foldl1' (<+?) [x<$several s | (x,s) <- levels])
  where levels = [(Quiet,"quiet"),(Verbose,"verbose"),(Debug,"debug")]
-- | Logs a line to stderr if the environment log level is greater than the given threshold
logLine :: MonadIO m => LogLevel -> String -> m ()
logLine level | envLogLevel>=level = \str -> liftIO $ logFile`seq`writeHString logFile (str+"\n")
              | otherwise = const unit

cyDebug :: Show a => a -> a
cyDebug | envLogLevel >= Debug = debug
        | otherwise = id

-- | A global handle to a log file (avoids reopening the same file over and over again)
logFile :: Handle
logFile = case envVar "" "CURLY_LOGFILE" of
  "" -> stderr
  f -> (openFile f AppendMode <*= \h -> hSetBuffering h LineBuffering)^.thunk

-- | A class for all types that can be formatted to a string
class Show a => FormatArg a where
  argClass :: a -> Char
  showFormat :: a -> String
  showFormat = show
-- | A base class for the 'format' function
class FormatType a where
  format' :: String -> String -> a
instance (FormatArg a,FormatType r) => FormatType (a -> r) where
  format' x ('%':c:t) a | c == argClass a = format' (reverse (showFormat a)+x) t
                        | otherwise = error "Invalid format argument type"
  format' x (c:t) a = format' (c:x) t a
  format' _ [] _ = error "Unused argument in format"
instance FormatType String where
  format' x t = reverse x+t
instance FormatArg Int where argClass _ = 'd'
instance FormatArg Float where argClass _ = 'f'
instance FormatArg Double where argClass _ = 'f'
instance FormatArg String where argClass _ = 's'; showFormat = id
instance FormatArg PortNumber where argClass _ = 'p'
instance Show (Pretty a) => FormatArg (Pretty a) where argClass _ = 'a'

-- | Runs an IO action, logging its errors if the given log level is lower than the environment
trylogLevel :: LogLevel -> IO a -> IO a -> IO a
trylogLevel l def = catch (\e -> logLine l (show e) >> def)
-- | Same as `tryLogLevel`, with a log level of `Debug`
trylog :: IO a -> IO a -> IO a
trylog = trylogLevel Debug
-- | A utility function that lifts its argument while logging its errors
liftIOLog :: MonadIO m => IO () -> m ()
liftIOLog = liftIO . trylogLevel Quiet unit

-- | A function that mimics sprintf-style formatting for Haskell
format :: FormatType r => String -> r
format = format' ""

-- | A global INotify instance
inotify = initINotify^.thunk
-- | Sets a watch on the given file, on the usual signals
watchFile s f = addWatch inotify [Modify,Create,Delete,Move,MoveIn,MoveOut,MoveSelf] s (\_ -> f)

-- | A utility function that opens a client socket to the given server and port
connectTo :: String -> PortNumber -> IO Handle
connectTo h p = trylog (error $ format "Couldn't connect to host %s:%p" h p) $ do
  connect . head =<< getAddrInfo Nothing (Just h) (Just (show p))

-- | Inclusive-or for `Map`s
(*+) :: (Ord k,Semigroup m) => Map k m -> Map k m -> Map k m
a *+ b = a*b+a+b

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
  ff'idents k (Compose x) = Compose<$>(traversel (traverse.ff'idents) k x >>= \x -> traversel ff'idents k (x :: f (g' a)))
instance HasIdents s s' (ExprNode s a) (ExprNode s' a) where
  ff'idents k (Lambda s a) = k s <&> \s' -> Lambda s' a
  ff'idents _ (Apply x y) = pure (Apply x y)
instance HasIdents s s' (s,a) (s',a) where
  ff'idents k (s,a) = map (,a) (k s)
instance HasIdents s s' t t' => HasIdents s s' (Maybe t) (Maybe t') where
  ff'idents = t'Just.ff'idents

-- | The type of all Curly builtins
data Builtin = B_Undefined
             | B_Seq
             | B_Unit 

             | B_Number Int
             | B_AddInt | B_SubInt | B_MulInt | B_DivInt
             
             | B_String String
             | B_StringLength
             | B_AddString | B_ShowInt

             | B_FileDesc Int
             | B_Open | B_Read | B_Write | B_Close
             deriving (Eq,Ord,Show,Generic)
instance Show (Pretty Builtin) where
  show (Pretty (B_Number n)) = show n
  show (Pretty (B_String s)) = show s
  show (Pretty b) = show b
instance Serializable Builtin where
instance Format Builtin where
instance NFData Builtin where rnf b = b`seq`()
