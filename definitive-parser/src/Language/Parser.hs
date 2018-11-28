-- |A module providing simple Parser combinator functionality. Useful
-- for small parsing tasks such as identifier parsing or command-line
-- argument parsing
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, AllowAmbiguousTypes, UndecidableInstances #-}
module Language.Parser (
  module Definitive,Proxy(..),
  -- * The ParserT Type
  ParserT(..),Parser,ParserA(..),i'ParserA,

  -- ** The Stream class
  Stream(..),emptyStream,

  -- ** Converting to/from Parsers
  parserT,parser,ioParser,matchesT,matches,readsParser,lookingAt,notLookingAt,

  -- * The MonadParser class
  MonadParser(..),ParseStreamType(..),ParseStream(..),
  
  -- * Basic combinators
  (>*>),(<*<),
  token,following,satisfy,
  oneOf,oneOfSet,noneOf,single,
  several,like,keyword,
  remaining,eoi,

  -- ** Specialized utilities
  readable,number,digit,letter,alNum,quotedString,space,nbspace,hspace,nbhspace,eol,
  
  -- * Useful combinators
  many,many1,sepBy,sepBy1,skipMany,skipMany1,
  many',many1',sepBy',sepBy1',skipMany',skipMany1',
  chainl,chainl',chainr,chainr',option,option',optionMaybe,optionMaybe'
  ) where

import Definitive hiding (take)

import Data.Char

-- | A proxy type to specify response types
data Proxy a = Proxy
             deriving (Show,Eq,Ord)

class ParseStreamType s where
  type StreamToken s :: *
  type StreamChar s :: *
instance ParseStreamType [a] where
  type StreamToken [a] = a
  type StreamChar [a] = a
  
class (ParseStreamType s, Stream (StreamToken s) s) => ParseStream s where
  completeBefore :: Proxy s -> StreamToken s -> Bool
  completeBefore _ _ = False
  tokenPayload :: Proxy s -> StreamToken s -> StreamChar s
  acceptToken :: StreamChar s -> s -> s
  acceptToken _ s = s
instance ParseStream [c] where
  tokenPayload _ c = c

class (MonadLogic m p,Monoid (p ()),Monad p,Monad m) => MonadParser s m p | p -> m s where
  runStreamState :: State s a -> p a
  tokenParser :: ParseStream s => (StreamToken s -> Either [StreamChar s] Bool) -> p (StreamChar s)
  tokenParser p = do
    (x,comp) <- runStreamState $ do
      x <- get
      let accept y = y <*= runStreamState . modify . acceptToken
      case (uncons :: s -> Maybe (StreamToken s,s)) x of
        Just (c,t) -> case p c of
          Left l -> return (l,accept)
          Right success -> put t >> return (if success then [tokenPayload (Proxy :: Proxy s) c] else [],id)
        Nothing -> return ([],id)
    comp (logicChoose x)
  noParse :: p a
  (<+?),(<+>) :: p a -> p a -> p a
  infixr 0 <+?,<+>
instance (MonadParser s m p,Monoid (p ((),s',Void))) => MonadParser s (StateT s' m) (StateT s' p) where
  runStreamState st = lift (runStreamState st)
  noParse = lift noParse
  (<+?) = by (stateT<.>stateT<.>stateT) (\a b s -> a s <+? b s)
  (<+>) = by (stateT<.>stateT<.>stateT) (\a b s -> a s <+> b s)
instance (MonadParser s m p,Monoid (p ((),Void,Void))) => MonadParser s (ReaderT s' m) (ReaderT s' p) where
  runStreamState st = lift (runStreamState st)
  noParse = lift noParse
  (<+?) = by (readerT<.>readerT<.>readerT) (\a b s -> a s <+? b s)
  (<+>) = by (readerT<.>readerT<.>readerT) (\a b s -> a s <+> b s)

newtype ParserT s m a = ParserT (StateT s (LogicT m) a)
                      deriving (Unit,Functor,Semigroup,Monoid,SemiApplicative,Applicative,
                                MonadFix,MonadError Void)
instance (Monad m,ParseStream s, StreamChar s ~ Char) => IsString (ParserT s m a) where
  fromString s = undefined <$ several s
instance Monad (ParserT s m) where join = coerceJoin ParserT
type Parser c a = ParserT c Id a
instance MonadTrans (ParserT s) where
  lift = ParserT . lift . lift
instance ConcreteMonad (ParserT s) where
  generalize = parserT %%~ map (pure.yb i'Id)
instance Monad m => MonadLogic (StateT s m) (ParserT s m) where
  deduce = coerceDeduce ParserT id
  induce = coerceInduce ParserT id
instance Monad m => MonadParser s (StateT s m) (ParserT s m) where
  runStreamState st = ParserT (generalize st)
  noParse = zero
  (<+?) = flip try
  (<+>) = (+)
deriving instance MonadWriter w m => MonadWriter w (ParserT s m)
deriving instance MonadReader r m => MonadReader r (ParserT s m)
i'ParserT :: Iso (ParserT s m a) (ParserT t n b) (StateT s (LogicT m) a) (StateT t (LogicT n) b)
i'ParserT = iso ParserT (\(ParserT p) -> p)
parserT :: (Monad n,Monad m) => Iso (ParserT s m a) (ParserT t n b) (s -> m [(s,a)]) (t -> n [(t,b)])
parserT = mapping listLogic.stateT.i'ParserT
parser :: Iso (Parser s a) (Parser t b) (s -> [(s,a)]) (t -> [(t,b)])
parser = mapping i'Id.parserT

readsParser :: Parser s a -> s -> [(a,s)]
readsParser p = p^..parser & map2 swap
lookingAt :: MonadParser s m p => p a -> p a
lookingAt p = do
  s <- runStreamState get
  p <* runStreamState (put s)
notLookingAt :: MonadParser s m p => p a -> p ()
notLookingAt p = do
  s <- runStreamState get
  optionMaybe' p >>= maybe unit (const noParse)
  runStreamState (put s)

ioParser :: Parser a b -> (a -> IO b)
ioParser p s = case (p^..parser) s of
  [] -> error "Error in parsing"
  (_,a):_ -> return a
matchesT :: (Monad f,Monoid m) => (a -> m) -> ParserT s f a -> s -> f m
matchesT f p s = foldMap (f . snd) <$> (p^..parserT) s
matches :: Monoid m => (a -> m) -> Parser s a -> s -> m
matches = map3 getId matchesT

-- |The @(+)@ operator with lower priority
(>*>) :: Monad m => ParserT a m b -> ParserT b m c -> ParserT a m c
(>*>) = (>>>)^..(i'ParserA<.>i'ParserA<.>i'ParserA)
(<*<) :: Monad m => ParserT b m c -> ParserT a m b -> ParserT a m c
(<*<) = flip (>*>)

newtype ParserA m s a = ParserA (ParserT s m a)
i'ParserA :: Iso (ParserA m s a) (ParserA m' s' a') (ParserT s m a) (ParserT s' m' a')
i'ParserA = iso ParserA (\(ParserA p) -> p)
parserA :: Iso (ParserA m s a) (ParserA m' s' a') (StateA (LogicT m) s a) (StateA (LogicT m') s' a') 
parserA = from stateA.i'ParserT.i'ParserA
instance Deductive (ParserA m) where
  (.) = (.)^.(parserA<.>parserA<.>parserA)
instance Monad m => Category (ParserA m) where
  id = ParserA (runStreamState get)
instance Monad m => Split (ParserA m) where
  (<#>) = (<#>)^.(parserA<.>parserA<.>parserA)
instance Monad m => Choice (ParserA m) where
  (<|>) = (<|>)^.(parserA<.>parserA<.>parserA)
instance Monad m => Arrow (ParserA m) where
  arr f = arr f^.parserA

-- |The remaining Stream to parse
remaining :: MonadParser s m p => p s
remaining = runStreamState get
-- |Consume a token from the Stream
token :: (ParseStream s,MonadParser s m p) => p (StreamChar s)
token = tokenParser (const (Right True))
following :: (ParseStream s,MonadParser s m p) => p (StreamToken s)
following = runStreamState get >>= \s -> case uncons s of
  Nothing -> noParse
  Just (c,_) -> return c

-- |Parse zero, one or more successive occurences of a parser
many :: MonadParser s m p => p a -> p [a]
many p = many1 p <+> pure []
-- |Parse one or more successiveé occurences of a parser
many1 :: MonadParser s m p => p a -> p [a]
many1 p = (:)<$>p<*>many p
-- |Parse zero, one or more successive occurences of a parser (no backtracking)
many' :: MonadParser s m p => p a -> p [a]
many' p = many1' p <+? pure []
-- |Parse one or more successiveé occurences of a parser (no backtracking)
many1' :: MonadParser s m p => p a -> p [a]
many1' p = (:)<$>p<*>many' p
-- |Skip many occurences of a parser
skipMany :: MonadParser s m p => p a -> p ()
skipMany p = skipMany1 p <+> pure () 
-- |Skip multiple occurences of a parser
skipMany1 :: MonadParser s m p => p a -> p ()
skipMany1 p = p >> skipMany p
-- |Skip many occurences of a parser (no backtracking)
skipMany' :: MonadParser s m p => p a -> p ()
skipMany' p = skipMany1' p <+? pure () 
-- |Skip multiple occurences of a parser (no backtracking)
skipMany1' :: MonadParser s m p => p a -> p ()
skipMany1' p = p >> skipMany' p

-- |Consume a token and succeed if it verifies a predicate
satisfy :: forall s m p. (MonadParser s m p,ParseStream s) => (StreamChar s -> Bool) -> p (StreamChar s)
satisfy p = tokenParser (\c -> Right $ p (tokenPayload (Proxy :: Proxy s) c))
-- |Consume a single fixed token or fail.
single :: forall s m p. (MonadParser s m p, Eq (StreamChar s), ParseStream s) => StreamChar s -> p ()
single c = void $ tokenParser (\c' -> if c==tokenPayload (Proxy :: Proxy s) c' then Right True else
                                  if completeBefore (Proxy :: Proxy s) c' then Left [c] else Right False)

-- |Consume a structure of characters or fail
several :: (Eq (StreamChar s), Foldable t, MonadParser s m p, ParseStream s) => t (StreamChar s) -> p ()
several l = traverse_ single l
-- |Consume a structure of characters or fail
like :: (Eq (StreamChar s), MonadParser s m p, ParseStream s) => [StreamChar s] -> p ()
like [] = return ()
like (c:t) = single c >> like' t
  where like' [] = return ()
        like' (c':t') = (single c' <+? unit) >> like' t'
-- |Consume a structure of characters or fail
keyword :: (Eq (StreamChar s), MonadParser s m p, Foldable t, ParseStream s) => a -> t (StreamChar s) -> p a
keyword a l = a <$ traverse_ single l

-- |Try to consume a parser. Return a default value when it fails.
option,option' :: MonadParser s m p => a -> p a -> p a
option a p = p <+> pure a
option' a p = p <+? pure a

optionMaybe,optionMaybe' :: MonadParser s m p => p a -> p (Maybe a)
optionMaybe p = option Nothing (map Just p)
optionMaybe' p = option' Nothing (map Just p)

-- |Succeed only at the End Of Input.
eoi :: (MonadParser s m p, ParseStream s) => p ()
eoi = remaining >>= guard.emptyStream
-- |The end of a line
eol :: (MonadParser s m p,ParseStream s, StreamChar s ~ Char) => p ()
eol = single '\n'

-- |Parse one or more successive occurences of a parser separated by
-- occurences of a second parser.
sepBy1 ::MonadParser s m p => p a -> p b -> p [a]
sepBy1 p sep = (:)<$>p<*>many (sep >> p)
-- |Parse zero or more successive occurences of a parser separated by
-- occurences of a second parser.
sepBy ::MonadParser s m p => p a -> p b -> p [a]
sepBy p sep = option [] (sepBy1 p sep)
-- |Parse one or more successive occurences of a parser separated by
-- occurences of a second parser (no backtracking)
sepBy1' :: MonadParser s m p => p a -> p b -> p [a]
sepBy1' p sep = (:)<$>p<*>many' (sep >> p)
-- |Parse zero or more successive occurences of a parser separated by
-- occurences of a second parser (no backtracking)
sepBy' :: MonadParser s m p => p a -> p b -> p [a]
sepBy' p sep = option' [] (sepBy1' p sep)

-- |Parse a member of a set of values
oneOf :: forall s t m p. (Eq (StreamChar s),Foldable t,ParseStream s,MonadParser s m p) => t (StreamChar s) -> p (StreamChar s)
oneOf s = tokenParser (\c -> if tokenPayload (Proxy :: Proxy s) c`elem`s then Right True else if completeBefore (Proxy :: Proxy s) c then Left (toList s) else Right False)
oneOfSet :: forall s m p. (Ord (StreamChar s),ParseStream s,MonadParser s m p) => Set (StreamChar s) -> p (StreamChar s)
oneOfSet s = tokenParser (\c -> if tokenPayload (Proxy :: Proxy s) c`isKeyIn`s then Right True else if completeBefore (Proxy :: Proxy s) c then Left (keys s) else Right False)
-- |Parse anything but a member of a set
noneOf :: (Eq (StreamChar s),Foldable t,ParseStream s,MonadParser s m p) => t (StreamChar s) -> p (StreamChar s)
noneOf t = satisfy (\e -> not (e`elem`t))

-- |Parse a litteral decimal number
number :: (MonadParser s m p,ParseStream s, StreamChar s ~ Char,Num n) => p n
number = fromInteger.read <$> many1' digit
-- |Parse a single decimal digit
digit :: (MonadParser s m p,ParseStream s, StreamChar s ~ Char) => p Char
digit = satisfy isDigit
alNum :: (MonadParser s m p,ParseStream s, StreamChar s ~ Char) => p Char
alNum = satisfy isAlphaNum
letter :: (MonadParser s m p,ParseStream s, StreamChar s ~ Char) => p Char
letter = satisfy isAlpha
-- |Parse a delimited string, using '\\' as the quoting character
quotedString :: (MonadParser s m p,ParseStream s, StreamChar s ~ Char) => Char -> p String
quotedString d = between (single d) (single d) (many ch)
  where ch = single '\\' >> unquote<$>token
             <+> noneOf (d:"\\")
        unquote 'n' = '\n'
        unquote 't' = '\t'
        unquote c = c
-- | Zero or more spaces
space :: (MonadParser s m p,ParseStream s, StreamChar s ~ Char) => p ()
space = option' () nbspace
-- | One or more spaces
nbspace :: (MonadParser s m p,ParseStream s, StreamChar s ~ Char) => p ()
nbspace = skipMany1' (satisfy (\x -> x==' ' || x=='\t' || x=='\n'))
-- | Zero or more horizontal spaces (no newlines)
hspace :: (MonadParser s m p,ParseStream s, StreamChar s ~ Char) => p ()
hspace = option' () nbhspace
-- | One or more horizontal spaces (no newlines)
nbhspace :: (MonadParser s m p,ParseStream s, StreamChar s ~ Char) => p ()
nbhspace = skipMany1' (satisfy (\x -> x==' ' || x=='\t'))

infixl 1 `sepBy`,`sepBy1`

-- |Chain an operator with an initial value and several tail values.
chainr :: MonadParser s m p => p a -> p (b -> a -> a) -> p b -> p a
chainr expr op e = compose<$>many (op<**>e)<*>expr
-- |Chain an operator with an initial value and several tail values.
chainr' :: MonadParser s m p => p a -> p (b -> a -> a) -> p b -> p a
chainr' expr op e = compose<$>many' (op<**>e)<*>expr
-- |Chain an operator with an initial value
chainl :: MonadParser s m p => p a -> p (a -> b -> a) -> p b -> p a
chainl expr op e = compose<$>many (flip<$>op<*>e)<**>expr
-- |Chain an operator with an initial value (eager)
chainl' :: MonadParser s m p => p a -> p (a -> b -> a) -> p b -> p a
chainl' expr op e = compose<$>many' (flip<$>op<*>e)<**>expr

-- |Test if a Stream is empty
emptyStream :: Stream c s => s -> Bool
emptyStream = maybe True (const False) . uncons

readable :: (Monad m,Read a) => ParserT String m a 
readable = generalize $ map2 swap (readsPrec 0)^.parser

