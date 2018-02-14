{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables, PatternSynonyms, CPP, TypeFamilies #-}
#if !MIN_VERSION_base(4,9,0)
{-# LANGUAGE DeriveDataTypeable #-}
#endif
module Curly.Core.Parser (
  -- * Expressions and operators
  OpMap,OpChar(..),OpParser,withParsedString,Severity(..),Warning(..),CurlyParserException(..),showWarning,l'library,
  Spaces(..),parseCurly,currentPos,spc,nbsp,
  expr,accessorExpr,tom,atom,

  -- * Basic blocks for warning generation  
  expected,opKeyword,guardWarn,warn,muteOnSuccess,

  -- * Lines and files
  curlyLine,curlyFile,
  ) where

import Curly.Core
import Curly.Core.Annotated
import Curly.Core.Documentation
import Curly.Core.Library
import IO.Filesystem
import Data.Char (isAlpha)
import Language.Format hiding (space,hspace)
import Control.Exception
import Data.Typeable (Typeable)

newtype ParseExpr s a = ParseExpr (((,) SourceRange :.: Free (ExprNode s:.:(,) SourceRange)) a)
                    deriving (Functor,Foldable,Unit,SemiApplicative,Applicative)
instance Monad (ParseExpr s) where join = coerceJoin ParseExpr
instance (Documented s,Documented a) => Documented (ParseExpr s a) where
  document e = document (semantic e :: Expression s a)
instance Traversable (ParseExpr s) where sequence = coerceSequence ParseExpr

type SourceExpr = ParseExpr String (String,Maybe (NameExpr GlobalID))

pattern PE :: (SourceRange, Free (ExprNode s :.: (,) SourceRange) a) -> ParseExpr s a
pattern PE e = ParseExpr (Compose e)
pattern PESym :: SourceRange -> a -> ParseExpr s a
pattern PESym r s = ParseExpr (Compose (r,Pure s))
pattern PEApp :: SourceRange
                 -> (SourceRange, Free (ExprNode s :.: (,) SourceRange) a)
                 -> (SourceRange, Free (ExprNode s :.: (,) SourceRange) a)
                 -> ParseExpr s a
pattern PEApp r f x = ParseExpr (Compose (r,Join (Compose (Apply f x))))
pattern PELam :: SourceRange
                 -> s
                 -> (SourceRange, Free (ExprNode s :.: (,) SourceRange) a)
                 -> ParseExpr s a
pattern PELam r s e = ParseExpr (Compose (r,Join (Compose (Lambda s e))))

impossible :: a
impossible = error "The impossible has happened"

instance Semantic (ParseExpr s a) s a where
  semNode = iso go back
    where go (PESym _ a) = SemSymbol a
          go (PELam _ s e) = SemAbstract s (PE e)
          go (PEApp _ f x) = SemApply (PE f) (PE x)
          go _ = impossible
          back (SemSymbol a)                          = PESym zero a
          back (SemApply (PE f@(r,_)) (PE x@(r',_)))  = PEApp (r+r') f x
          back (SemApply _ _)                         = impossible
          back (SemAbstract s (PE e@(r,_)))           = PELam r s e
          back (SemAbstract _ _)                      = impossible

data OpChar = OC_Char Char
            | OC_CompleteChar Char
data OpStream = OpStream [Char] [(OpChar,(Char,Int,Int,Int))]
instance Semigroup OpStream where
  OpStream h t + OpStream h' t' = OpStream (h+h') (t+t')
instance Monoid OpStream where
  zero = OpStream zero zero 
instance Stream OpChar OpStream where
  cons c (OpStream h l) = OpStream h ((c,('\0',0,0,0)):(l & set (t'head.l'2.l'1)
                                                        (case c of OC_Char cc -> cc ; _ -> '\0')))
  uncons (OpStream _ []) = Nothing
  uncons (OpStream h ((c,_):l)) = Just (c,OpStream (case c of OC_Char c' -> c':h ; _ -> h) l)
instance ParseToken OpChar where
  type TokenPayload OpChar = Char
  completeBefore (OC_CompleteChar _) = True
  completeBefore _ = False
  tokenPayload (OC_Char c) = c
  tokenPayload (OC_CompleteChar c) = c
instance ParseStream OpChar OpStream where
  acceptToken c (OpStream h t) = OpStream (c:h) t

mkStream :: (ParseToken c, Stream c s, TokenPayload c ~ Char) => s -> OpStream
mkStream = OpStream "" . mk ('\0',0,0,0)
  where mk (p,n,ln,cl) s = case uncons s of
          Just (c,s') -> nextChar (tokenPayload c) s'
          Nothing -> []
          where nextChar '\n' s' = (OC_Char '\n',(p,n,ln,cl)):mk ('\n',n+1,ln+1,0) s'
                nextChar c    s' = (OC_Char c,(p,n,ln,cl)):mk (c,n+1,ln,cl+1) s'

type OpMap_Val = ((Int,Bool),String)
newtype OpMap = OpMap { getOpMap :: Cofree (Map Char) (Maybe OpMap_Val) }
i'OpMap :: Iso' (Cofree (Map Char) (Maybe OpMap_Val)) OpMap
i'OpMap = iso OpMap getOpMap
type OpParser m = ParserT OpStream (RWST Void [Warning] (Int,OpMap,(Map String (NameExpr GlobalID),Library)) m)
data Spaces = HorizSpaces | AnySpaces

class (MonadParser s m p, ParseStream c s, TokenPayload c ~ Char) => MonadCharParser c s m p

instance (Monad m, ParseStream c s, TokenPayload c ~ Char) => MonadCharParser c s (StateT s m) (ParserT s m)
         
parseSpaces :: MonadCharParser c s m p => Spaces -> p ()
parseSpaces HorizSpaces = hspc
parseSpaces AnySpaces = spc
parseNBSpaces :: MonadCharParser c s m p => Spaces -> p ()
parseNBSpaces HorizSpaces = nbhsp
parseNBSpaces AnySpaces = nbsp

withParsedString :: Monad m => OpParser m a -> OpParser m (String,a)
withParsedString ma = do
  h <- runStreamState (id <~ \(OpStream h l) -> (OpStream [] l,h))
  a <- ma
  h' <- runStreamState (id <~ \(OpStream h' l) -> (OpStream (h'+h) l,reverse h'))
  return (h',a)

instance Semigroup OpMap where
  OpMap (Step x y) + OpMap (Step x' y') = OpMap (Step (x+x') (map getOpMap (map OpMap y*+map OpMap y')))
instance Monoid OpMap where
  zero = OpMap (Step zero zero)
instance DataMap OpMap String OpMap_Val where
  at [] = from i'OpMap.l'1
  at (c:cs) = from i'OpMap.l'2.at c.mapping i'OpMap.l'Just zero.at cs

data Severity = Sev_Info | Sev_Error
instance Show Severity where
  show Sev_Info = "Info"
  show Sev_Error = "Error"
data Warning = Warning Severity (Int,Int) String
             deriving (Show,Typeable)
data CurlyParserException = CurlyParserException (Maybe String) [Warning]
                          deriving (Show,Typeable)
instance Exception CurlyParserException
instance Documented CurlyParserException where
  document (CurlyParserException s ws) =
    Pure $ intercalate "\n" [format "  #%d:%s" (i :: Int) (showWarning s w) | (i,w) <- zip [1..] ws]

showWarning :: Maybe String -> Warning -> String
showWarning f (Warning sev (l,c) s) = format "%s %s%d:%d: %s" (show sev) (c'string $ maybe "" (format "%s: ") f) l c s
warn :: Monad m => Severity -> String -> OpParser m ()
warn sev s = currentPos >>= \(_,l,c) -> tell [Warning sev (l,c) s]
muteOnSuccess :: Monad m => OpParser m a -> OpParser m a
muteOnSuccess p = do
  (ws,ma) <- intercept (option' Nothing (Just<$>p))
  case ma of
    Nothing -> tell ws >> zero
    Just a -> tell [w | w@(Warning Sev_Info _ _) <- ws] >> return a
expected :: Monad m => String -> OpParser m a -> OpParser m a
expected x p = p <+? (warn Sev_Error (format "expected %s" x) >> zero)
opKeyword :: Monad m => String -> OpParser m ()
opKeyword s = expected (format "opKeyword '%s'" s) (several s)
guardWarn :: Monad m => Severity -> String -> Bool -> OpParser m ()
guardWarn sev msg p = if p then unit else (warn sev msg >> zero)

l'library :: Lens a b (x,y,(z,a)) (x,y,(z,b))
l'library = l'3.l'2
l'typeMap :: Lens a b (x,y,(a,z)) (x,y,(b,z))
l'typeMap = l'3.l'1

parseCurly :: (ParseStream c s, TokenPayload c ~ Char,Monad m) => s -> OpParser m a -> m ([Warning]:+:a)
parseCurly s p = (deduce p^..mapping i'RWST.stateT) (mkStream s) zero <&> \((_,ma),_,ws) -> case ma of
  Just (a,_) -> Right a
  Nothing -> Left ws

mkRange :: SourcePos -> SourcePos -> SourceRange
mkRange p p' = SourceRange Nothing p p'

mkLet :: Semantic e i o => Either (i, Maybe e) [e] -> e -> e
mkLet (Left (s,v)) = maybe id (flip mkApply) v . mkAbstract s
mkLet (Right t) = \e -> foldl1' mkApply (t+[e])


space, spc, hspc, nbsp, nbhsp, hspace 
  :: MonadCharParser c s m p => p ()
space = hspace + (eol >> skipMany' (single '#' >> skipMany' (satisfy (/='\n')) >> eol))
hspace = void $ oneOf [' ', '\t']
spc = skipMany' space
hspc = skipMany' hspace
nbsp = skipMany1' space
nbhsp = skipMany1' hspace

floating, swaying, wrapRound, wrapCurly :: Monad m => OpParser m a -> OpParser m a
floating = between spc spc
swaying = between hspc hspc
wrapRound = between "(" (expected ")" ")") . floating
wrapCurly = between "{" (expected "}" "}") . floating

previousChar :: Monad m => OpParser m Char
previousChar = remaining <&> \(OpStream _ s) -> case s of
  [] -> '\0'
  ((_,(p,_,_,_)):_) -> p
currentPos :: Monad m => OpParser m (Int,Int,Int)
currentPos = remaining <&> \(OpStream _ s) -> case s of
  [] -> (0,0,0)
  ((_,(_,n,l,c)):_) -> (n,l,c)

name :: Monad m => OpParser m String
name = do
  pr <- previousChar
  guard (not (isLetter pr)) >> many1' (satisfy isLetter <+? qChar)
    <+? guard (not (isOperator pr)) >> many1' (satisfy isOperator <+? qChar)
  where qChar = single '\\' >> token

isOperator, isLetter :: Char -> Bool
isOperator c = not (elem c (c'set $ fromKList "{(_\"')} \t\n\\") || isLetter c)
isLetter c = isAlpha c || inRange '0' '9' c || c=='\''
edgeName :: Bool -> String
edgeName t = if t then "_" else ""
mkSymName :: Bool -> [String] -> Bool -> Bool -> String
mkSymName l s r isR = edgeName l + intercalate "_" s + edgeName r + edgeName isR

symEdge :: Monad m => OpParser m Bool
symEdge = option' False (True<$"_")

varName :: Monad m => OpParser m String
varName = liftA4 mkSymName symEdge (name`sepBy1'`"_") symEdge symEdge >>= \n ->
  if isKeyIn (last n) (c'set $ fromKList ":=")
  then init n <$ runStreamState (modify (cons (OC_Char (last n)))) <*= guard . nonempty
  else return n

mkSymIn :: Semantic e i (String,Maybe (NameExpr GlobalID)) => Map String (NameExpr GlobalID) -> String -> e
mkSymIn m = \n -> mkSymbol (n,lookup n m)

tom, expr, accessorExpr :: Monad m => Spaces -> OpParser m SourceExpr
expr sp = foldl1' mkApply<$>sepBy1' (tom sp) (parseNBSpaces sp) 
accessorExpr sp = expr sp <*= \e -> defAccessors (map fst (toList e))

-- completing :: Monad m => OpParser Id a -> OpParser m [(String,a)]
-- completing p = do
--   OpStream _ t <- runStreamState get
--   st <- lift get
--   let Id (ret,_,_) = (p^..mapping i'RWST.parserT) (OpStream "" t) (undefined,st)
--   return [(reverse h,x) | (OpStream h _,x) <- ret]

tom sp = do
  OpMap (Step _ opmap) <- lift (getl l'2)
  typeMap <- lift (getl l'typeMap)
  let param m c = case m^.at c of Just tl -> return tl ; _ -> zero
      tokParam m = multi <+? (param m =<< (oneOfSet (delete '_' $ keysSet m) <*= guard . (/='_')))
        where multi = case m^.at ' ' of
                Just tl -> tl <$ nbsp
                _ -> zero

      operation _sp p = opSuf
        where opSuf = do
                pref <- opPref
                case opmap^.at '_' of
                  Just tl -> flip fix pref $ \mkSuf (d,e) ->
                    (parseSpaces _sp >> suffix (liftA2 (&&) p (<=d)) (e:) tl >>= mkSuf)
                    <+? return (d,e)
                  _ -> return pref
              opPref = (tokParam opmap >>= suffix (>=0) id) <+? map ((maxBound :: Int,) . mkSymbol . Just) atom
                       <+? (maxBound,mkSymbol Nothing)<$"_"

      mkOp n = foldl' mkApply (mkSymbol (Just (mkSymIn typeMap n)))
      suffix p argPrefix = suff
        where
          filterM cmp = snd . go
            where go (Step x xs) = let x' = x <*= guard . cmp . fst . fst
                                       xs' = map go xs
                                       isEmpty = not (any fst xs')
                                   in (maybe False (const True) x' || not isEmpty,
                                       Step x' (if isEmpty then zero else map snd xs'))
          suff (Step (Just x@((d,_),_)) m) =
            suff (filterM (>= d) (Step Nothing m))
            <+? suffO x
            <+? suff (filterM (< d) (Step Nothing m))
          suff (Step Nothing m) = suffM m
          suffM m = (tokParam m >>= suff) <+? do
            let exprSuf tl@(Step _ tlm) | empty tlm = zero
                                        | otherwise = between spc spc (operation AnySpaces (>=0))
                                                      >>= \(_,e) -> suffix p (argPrefix . (e:)) tl
            tl <- param m '_'
            guard (any (maybe False (p . fst . fst)) tl)
            case tl of
              Step (Just ((d,isR),n)) _ | p d -> do
                foldr1 (<+?) [ exprSuf (filterM (> d) tl)
                             , spc >> operation sp (if isR then (>=d) else (>d))
                               <&> \(d',e) -> (min d d',mkOp n (argPrefix [e]))
                             , exprSuf (filterM (> d) tl)]
              _ -> exprSuf tl
          suffO ((d,_),n) | p d = return (d,mkOp n (argPrefix []))
          suffO _ = zero

      trim x = case sem x of
        SemAbstract s e | matches (\(_ :: Int) -> True) (single '#' >> number) s ->
          let e' = trim e
          in case sem e' of
            SemApply f (PatSymbol (s',_)) | s==s' -> f
            _ -> mkAbstract s e'
        _ -> x
      fillHoles e = foldr mkAbstract (mapAccum_ go e (0 :: Int)) holes
        where holes = ["#"+show (i :: Int) | i <- [0..foldMap (maybe 1 (const 0)) e-1]]
              go Nothing i = (i+1,mkSymbol ("#"+show i,Nothing))
              go (Just e') i = (i,e')

  trim . join . fillHoles . snd <$> operation sp (>=0)

atom :: Monad m => OpParser m SourceExpr
atom = withPostfix
       =<< wrapCurly (expected "lambda-expression" lambda) <+? wrapRound (expr AnySpaces) <+? (expected "symbol" close)
  where
    close = liftA2 (&) (liftA2 mkSymIn (lift $ getl l'typeMap) name <+? string '"' <+? string '\'')
            $ option' id $ wrapRound $ do
      sepBy1' (tom AnySpaces) nbsp <&> \args e -> foldl' mkApply e args
    withPostfix s = foldl' (\e n -> mkApply (mkSymbol ('.':n,Nothing)) e) s
                    <$> many' (single '.' >> many1' letter)
    string delim = between (single delim) (single delim) $ mkConcat . g . foldr f ("",[]) <$> many' stringExpr
      where stringExpr = map Left (single '$' >> wrapCurly (expr AnySpaces))
                         <+? map Right (single '\\' >> unquote<$>token <+? satisfy (/=delim))
            unquote 'n' = '\n'
            unquote 't' = '\t'
            unquote '0' = '\0'
            unquote c = c
            mkConcat [] = mkSymbol ("\"\"",Nothing)
            mkConcat l = foldl1' (++^) l
              where e ++^ e' = (mkSymbol ("#concat",Nothing)`mkApply`e)`mkApply`e'
            f (Left e) x = ("",e:g x)
            f (Right c) ~(s,es) = (c:s,es)
            g ("",es) = es
            g (s,es) = mkSymbol (delim:(s+[delim]),Nothing):es
    
    lambda = do
      old <- lift get
      args <- fold <$> sepBy1' lambdaArg nbsp
      _ <- floating ":"
      e <- expr AnySpaces
      lift (put old)
      return $ foldr mkLet e args

lambdaArg :: Monad m => OpParser m [(String, Maybe SourceExpr):+:[SourceExpr]]
lambdaArg = letBinding + funPrefix + do
  n <- (varName <*= register) <+? ("_"<$"_")
  tm <- lift (getl (l'typeMap))
  map (Left (n,Nothing):) $ option' [] $
    map (Right [mkSymIn tm n]:) $ wrapRound (map fold (sepBy' lambdaArg nbsp))
  where letBinding = wrapCurly $ expected "let-binding" $ typeBinding <+? do
          n <- varName
          old <- lift get
          args <- fold <$> many' (nbsp >> lambdaArg)
          _ <- floating (opKeyword "=")
          e <- expr AnySpaces
          lift (put old)
          register n
          return [Left (n,Just (foldr mkLet e args))]
        typeBinding = do
          (_,_,(_,_),(ctor,ctt),(dtor,dtt)) <- typeDecl
          lift $ l'typeMap =~ insert ctor (typeExpr ctt) . insert dtor (typeExpr dtt)
          register ctor ; register dtor
          return []
        funPrefix = wrapRound $ pure . Right<$>sepBy1' (tom AnySpaces) nbsp

typeExpr :: Type GlobalID -> NameExpr GlobalID
typeExpr t = mkAbstract (pureIdent "#0") (mkSymbol (pureIdent "#0",Pure (Argument 0))) & from i'NameNode.t'Join.annType %- t

curlyFile :: (Monad m, ?mountain :: Mountain) => OpParser m Library
curlyFile = do
  modFile + symFile
  when (envLogLevel >= Debug) (mtrace "Finished parsing")
  lift (getl l'library)
  where modFile = do
          _ <- "#!/lib/module!#" <+? "module"
          syn <- hspc *> synopsis <* (eol+eoi)
          lift (l'library.metadata =~ syn)
          skipMany' (muteOnSuccess (curlyLine <+> (hspc >> eol)))
        synopsis = liftA2 (.) (option' id (synName <* ":" <* hspc))
                   (many' (noneOf' "\n") <&> \s -> insert "synopsis" (Pure s))
          where synName = liftA2 (.) (many1' (noneOf' ": \t") <&> \s -> insert "name" (Pure s))
                          (option' id (versionIntro >> many1' (noneOf' ": \t") <&> \v -> insert "version" (Pure v)))
                versionIntro = ((nbhsp >> option' () "v") <+? "-")
        noneOf' = noneOf . c'string
        symFile = do
          mods <- ("#!/lib/symbol!#" <+? "symbol") >> swaying (modTree`sepBy'`nbhsp) <* (eol+eoi)
          mods' <- traverse resolve mods
          pre <- currentPos
          e <- floating (expr AnySpaces)
          post <- currentPos
          lift $ l'library =~ compose [
            addImport (fold mods'),
            defSymbol "value" (mkRange pre post) Nothing False e,
            setExports (Pure "value")]

raw :: (ParseStream c s, TokenPayload c ~ Char,MonadParser s m p) => String -> p ()
raw = several

curlyLine :: (Monad m, ?mountain :: Mountain) => OpParser m ()
curlyLine = expected "Curly source definition ('define', 'type', 'family', 'import', 'export', ...)"
            (swaying (foldr1 (<+?) [defLine,descLine,typeLine,classLine,comment,impLine,expLine,transLine,setLine,metaLine])
             <* expected "end of line" (eol+eoi)
             >>= \f -> lift $ l'library =~ f)
  where withPlural p = p >> option' () "s"
        impLine = withPlural "import" >> nbsp >> do
          mods <- sepBy1' modTree nbhsp
          resolved <- fold<$>traverse resolve mods
          guardWarn Sev_Info (format "Nothing to import for '%s' in the current context" (show mods)) (nonempty resolved)
          let newinsts = c'set $ fromKList $ fold [toList (zipWith const ?mountain m) | m <- mods]
              addID fl (GlobalID n Nothing) = GlobalID n (Just (n,fl^.flID))
              addID _  i = i
              c x = x :: InstanceMap GlobalID (Maybe LibraryID,LeafExpr GlobalID)
              ret = addImport resolved
              ret' = implicits %~ (+ fold [map (first (+ Just (fl^.flID)))
                                           $ c $ warp (traverse.l'2.ff'idents) (addID fl)
                                           $ c $ warp ff'idents (addID fl)
                                           $ fl^.flLibrary.implicits
                                          | fl <- keys newinsts])
          return (ret' . ret)
        expLine = withPlural "export" >> nbsp >> do
          mods <- sepBy1' modTree nbhsp
          let mkExp (_,Just s) = s
              mkExp (s,_) = s
          return (composing (\mod' -> addExport (map mkExp mod')) mods)
        transLine = withPlural "transport" >> nbsp >> do
          mods <- sepBy1' modTree nbhsp
          mods' <- fold<$>traverse resolve mods
          return (addExport (map (identName . fst) mods') . addImport mods')
        metaLine = "module" >> nbsp >> do
          ph:pt <- sepBy1' (many1' (noneOf [' ','\t','\n']) <*= guard . (/=":")) nbsp
          nbsp >> ":" >> nbsp
          v <- many1' (noneOf ['\n'])
          return (metadata.at ph.l'Just (Join zero) %~ insert pt (Pure v))
        defLine = "define" >> nbsp >> setLine
        setLine = (previousChar >>= \c -> guard (c==' '||c=='\t')) >> do
          sym <- (("." >> ('.':) <$> many1' letter) <+? varName)
          old <- lift get
          args <- fold <$> many' (nbsp >> lambdaArg)
          _ <- floating (opKeyword "=")
          pre <- currentPos
          e <- expr HorizSpaces
          post <- currentPos
          lift (put old)
          register sym
          defAccessors (sym:map fst (toList e))
          return $ \l -> case l^.symbols.at sym of
            Just lf | lf^.leafIsMethod ->
                      let e' = optExprIn l (foldr mkLet e args)
                          t' = lf^.leafType
                               + mapTypePathsMonotonic (Just . warp (l'1.t'ImplicitRoot) (+1)) (exprType e')
                          ((cn,is):_,_) = typeConstraints t'
                          lf' = lf & set leafType t' . set leafVal e'
                      in l & compose [implicits %~ insert (cn,i,t') (Nothing,lf') | i <- is]
            _ -> defSymbol sym (mkRange pre post) Nothing False (foldr mkLet e args) l
        descLine = "describe" >> nbsp >> do
          n <- varName
          nbsp >> opKeyword "as" >> nbsp
          d <- docLine "doc" []
          return (descSymbol n d)
        typeLine = do
          (tname,(cname,arity),(pre,post),(ctor,ctt),(dtor,dtt)) <- typeDecl
          let rng = mkRange pre post
              tt = constraintType (pureIdent tname) arity
              
          register ctor; register dtor; register tname
          return (defTypeSym ctor False rng ctt (expr_constructor ctt)
                  . defTypeSym dtor False rng dtt (expr_destructor dtt)
                  . defTypeSym cname False rng tt (compose [expr_constant | _ <- [1..arity]] expr_identity))
        classLine = "family" >> do
          cl <- nbsp >> varName
          indices <- many' $ between "[" "]" $ sepBy1' varName ","
          args <- many' (nbsp >> varName <*= guard . (/=":"))
          _ <- nbsp >> opKeyword ":"
          (pre,tp,post) <- nbsp >> typeSum
          defClass cl args indices (mkRange pre post) tp
        comment = id <$ raw "#" <* skipMany' (satisfy (/='\n'))

defClass :: Monad m => String -> [String] -> [[String]] -> SourceRange
            -> (Library -> Type GlobalID)
            -> OpParser m (Library -> Library)
defClass cl args indices range tp = do
  l <- lift (getl l'library)
  let l' = defRigidSymbols args l
      argMap = c'map (fromAList (zip args [0..]))
      index i = convert (lookup i argMap)
      fullIndices | empty indices = [args]
                  | otherwise = indices
      t = abstractImplicitType (pureIdent cl,map (fromKList . (>>= index)) fullIndices) args (tp l')
  register cl
  return (defTypeSym cl True range t expr_identity)

defAccessors :: Monad m => [String] -> OpParser m ()
defAccessors syms = do
  l <- lift (getl l'library)
  for_ [a | a@('.':_) <- syms] $ \ac ->
    unless (isKeyIn ac (l^.symbols)) $
    let sym n = mkSymbol (n,Nothing)
        infixl 9 !    ; infixr 2 />
        (!) = mkApply ; (/>) = mkAbstract
        e = "x" /> (("y" /> sym "b" ! sym "...")
                    ! (sym "a"!sym "x"))
    in do
      patch <- defClass ac ["a","b"] [["a"]] NoRange $ \l' ->
        exprType $ exprIn l' (e :: SourceExpr)
      lift (l'library =~ patch)

defTypeSym :: String -> Bool -> SourceRange -> Type GlobalID -> RawNameExpr GlobalID -> Library -> Library
defTypeSym n isM rng tp e = symbols.at n.l'Just (undefSymLeaf n Nothing) %~
                            set leafVal (set t'exprType tp (_rawNameExpr e))
                            . set leafPos rng
                            . set leafType tp . set leafIsMethod isM

defRigidSymbols :: [String] -> Library -> Library
defRigidSymbols args = compose [defTypeSym a False NoRange (rigidTypeFun a) expr_identity
                               | a <- args]

typeSum :: Monad m => OpParser m (SourcePos,Library -> Type GlobalID,SourcePos)
typeSum = do
  let typeNode = (fill Nothing delim <+? map Just (tom HorizSpaces)) >>= maybe zero pure
      delim = between hspc nbsp ("and"<+?oneOf (c'string "&|"))
  pre <- currentPos
  exprs <- sepBy1' (foldl1' mkApply <$> sepBy1' typeNode nbhsp) delim
  post <- currentPos
  return (pre,(\l -> foldl1' (+) [exprType (exprIn l e) | e <- exprs]),post)

typeDecl :: Monad m => OpParser m (String,(String,Int),
                                   (SourcePos, SourcePos),
                                   (String, Type GlobalID),
                                   (String, Type GlobalID))
typeDecl = "type" >> nbsp >> do
  mctor <- optionMaybe' (varName <* nbsp <* opKeyword ":" <* nbsp)
  mcstr <- optionMaybe' (varName <* nbsp <* opKeyword ":" <* nbsp)
  tname <- varName
  cargs <- many' (nbsp *> varName <*= guard . (/="="))
  let ctor = fromMaybe tname mctor
      cstr = fromMaybe ("c'"+ctor) mcstr
  nbsp >> opKeyword "=" >> nbsp
  dtor <- varName
  dargs <- many' (nbsp *> varName <*= guard . (/=":"))
  let args = cargs+dargs
  nbsp >> opKeyword ":" >> nbsp
  old <- lift get
  traverse_ register args
  (pre,tp,post) <- typeSum
  lift (put old)
  l <- lift $ getl l'library
  let tp_l = tp (defRigidSymbols args l)
      (tpc,tpd) = abstractStructTypes (pureIdent tname) cargs dargs tp_l
  return (tname,(cstr,length cargs),(pre,post),(ctor,tpc),(dtor,tpd))

(<##>) :: Lens s t a a -> Lens s' t' a b -> Lens (s,s') (t,t') a b
l1 <##> l2 = lens (liftA2 (,) (by l1) (by l2)) (\a (t,t') -> set l2 t' (set l1 t a))

register :: Monad m => String -> OpParser m ()
register "_" = unit
register n = when (elem '_' n) $ lift (l'1<##>l'2 =~ \(d,m) -> if elem '_' n
                                                               then (d+1,insert nk ((d,isR),n) m)
                                                               else (d,insert n ((maxBound,False),n) m))
  where isR = drop (length n-2) n == "__"
        nk = if isR then init n else n
        
resolve :: (Monad m, ?mountain :: Mountain) => Module (String, Maybe String) -> OpParser m Context
resolve m = m' <$ traverse (register.identName.fst) m'
  where m' = zipWith r m localContext
        r (_,Just n) (GlobalID _ l,v) = (GlobalID n l,v)
        r _ x = x
modTree :: Monad m => OpParser m (Module (String,Maybe String))
modTree = tree <&> \x -> Join (ModDir [x])
  where modName = many1' (satisfy (\c -> not (c`elem`c'set (fromKList " \t\n{()}.\\"))) <+?
                          (raw "\\" >> token))
        tree = sepBy1' modName "." >>= \ (nh:nt) -> do
          let modN n e = Join (ModDir [(n,e)])
              modNs e = foldr modN e nt
              ln = last (nh:nt)
          map ((nh,) . modNs) $ option' (Pure (ln,Nothing)) $ hspc *> do
            Join . ModDir <$> wrapCurly (tree`sepBy'`nbsp)
              <+> Pure . (ln,) . Just <$> wrapRound modName

