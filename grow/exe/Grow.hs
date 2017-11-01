{-# LANGUAGE TypeFamilies #-}
module Main where

import Definitive
import Language.Parser
import Data.Time.Clock.POSIX
import System.Posix.Files
import System.Directory (getModificationTime)

modTime :: String -> IO TimeStamp
modTime p = TimeStamp <$> try (return 0) (getModificationTime p <&> realToFrac . utcTimeToPOSIXSeconds)

data TimeStamp = TimeStamp Double
instance Semigroup TimeStamp where
  TimeStamp a + TimeStamp b = TimeStamp (max a b)
instance Monoid TimeStamp where
  zero = TimeStamp 0
instance Show TimeStamp where show (TimeStamp t) = show t

newtype GrowT m a = GrowT { runGrowT :: RWST (GrowEnv m) TimeStamp () m a }
                  deriving (Functor, Unit, SemiApplicative, Applicative, MonadFix,
                            MonadReader (GrowEnv m), MonadWriter TimeStamp, MonadFS)
instance MonadFS IO where
  getFileContents = readString
  getFileModificationTime = modTime
instance (Monoid w,MonadFS m) => MonadFS (RWST r w s m)
instance MonadTrans GrowT where
  lift ma = GrowT (lift ma)
instance Monad m => Monad (GrowT m) where
  join = coerceJoin GrowT

class Monad m => MonadFS m where
  getFileContents :: String -> m String
  getFileModificationTime :: String -> m TimeStamp
  default getFileContents :: (MonadFS n, MonadTrans t, m ~ t n) => String -> m String
  getFileContents f = lift (getFileContents f)
  default getFileModificationTime :: (MonadFS n, MonadTrans t, m ~ t n) => String -> m TimeStamp
  getFileModificationTime f = lift (getFileModificationTime f)
  

data GrowNode a = GrowMap (Map String a)
                | GrowFunc String a
                | GrowApply a a
                | GrowDelayed a
data Builtin m = B_In_0 | B_In_1 (GrowThunk m) | B_Val
data GrowLeaf m = GrowString String | GrowBuiltin (Builtin m)
type GrowThunk m = Free (GrowNode :.: GrowT m) (GrowLeaf m)
type GrowEnv m = Map String (GrowThunk m)

hnf :: MonadFS m => GrowThunk m -> GrowT m (GrowThunk m)
hnf (Pure l) = pure (Pure l)
hnf th@(Join (Compose n)) = case n of
  GrowApply mf mx -> (hnf =<< mf) >>= \f -> do
    case f of
      Pure (GrowBuiltin B_In_0) -> pure $ Pure $ GrowBuiltin $ B_In_1 (Join . Compose $ GrowDelayed mx)
      Pure (GrowBuiltin (B_In_1 e)) -> do
        Join (Compose (GrowMap m)) <- hnf =<< mx
        local (map (Join . Compose . GrowDelayed) m+) (hnf e)
      Pure (GrowBuiltin B_Val) -> do
        Pure (GrowString s) <- hnf =<< mx
        env <- ask
        case lookup s env of
          Just v -> hnf v
          Nothing -> do
            (ts,f) <- liftA2 (,) (getFileModificationTime s) (getFileContents s) 
            tell ts
            return (Pure (GrowString f))
      Pure s -> error "Cannot apply a string"
      Join (Compose (GrowFunc s a)) ->
        local (\env -> trace (show $ keys env) $ insert s (Join . Compose $ GrowDelayed (local (const env) (hnf =<< mx))) env) (hnf =<< a)
      Join (Compose (GrowDelayed x)) -> hnf (Join (Compose (GrowApply x mx)))
  GrowDelayed me -> hnf =<< me
  _ -> pure th

growExpr :: (ParseStream Char s,MonadParser s n m,Monad o) => m (GrowThunk o)
growExpr = between sp sp expr
  where expr = dict <+? letExpr <+? dollar <+? map (Pure . GrowString) string
               <+? between (single '(' >> sp) (sp >> single ')') (sepBy1' expr nbsp <&> foldl1' ap)
        dollar = do
          single '$'
          e <- expr
          return (Join (Compose (GrowApply (pure (Pure (GrowBuiltin B_Val))) (pure e))))
        letExpr = do
          several "let"; nbsp
          dict <- expr
          between nbsp nbsp (several "in")
          e <- expr
          return (foldl1' ap [Pure (GrowBuiltin B_In_0),e,dict])
        dict = between (single '{' >> sp) (sp >> single '}') $ do
          assocs <- dictAssoc`sepBy'`between sp sp (single ';')
          return (Join (Compose (GrowMap $ map pure $ fromAList assocs)))
        dictAssoc = do
          n <- string
          between sp sp (single '=')
          map (n,) $ expr
        string = quotedString '"' <+? many1' (satisfy (\x -> inside 'a' 'z' x || inside 'A' 'Z' x || inside '0' '9' x))
        spc = satisfy (`elem`" \t\n")
        sp = skipMany' spc
        nbsp = spc >> sp
        ap f x = Join (Compose (GrowApply (pure f) (pure x)))

builtinFunctions = [
  ("strip", Join $ Compose $ GrowFunc "x" $ do
       Just x <- lookup "x" <$> ask
       Pure (GrowString s) <- hnf x
       let strip = dropWhile (`elem`" \t\n")
       return (Pure (GrowString (strip (reverse (strip (reverse s))))))
  )
  ]

main = do
  file <- readString "Seed"
  case matches Just growExpr file of
    Just e -> (runRWST . runGrowT) (hnf e) (fromAList builtinFunctions,()) >>= \(a,_,ts) -> do
      case a of
        Pure (GrowString s) -> putStrLn (show ts + ": " + s)
        _ -> putStrLn "..."
