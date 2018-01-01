{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables, PatternSynonyms, CPP, TypeFamilies #-}
#if !MIN_VERSION_base(4,9,0)
{-# LANGUAGE DeriveDataTypeable #-}
#endif
module Curly.Core.Parser (
  -- * Expressions and operators
  OpMap,OpParser,Warning(..),CurlyParserException(..),showWarning,l'library,
  Spaces,parseCurly,currentPos,hspace,space,spc,hspc,nbsp,nbhsp,
  expr,accessorExpr,tom,atom,

  -- * Basic blocks for warning generation  
  expected,opKeyword,guardWarn,

  -- * Lines and files
  curlyLine,curlyFile,
  ) where

import Curly.Core
import Curly.Core.Annotated
import Curly.Core.Library
import IO.Filesystem
import Data.Char (isAlpha)
import Language.Format hiding (space)
import Control.Exception
import Data.Typeable (Typeable)

instance IsString (Set Char) where
  fromString = fromKList

newtype ParseExpr s a = ParseExpr (((,) SourceRange :.: Free (ExprNode s:.:(,) SourceRange)) a)
                    deriving (Functor,Foldable,Unit,SemiApplicative,Applicative)
instance Monad (ParseExpr s) where join = coerceJoin ParseExpr
instance (Show (Pretty s),Show (Pretty a)) => Show (Pretty (ParseExpr s a)) where
  show (Pretty e) = pretty (semantic e :: Expression s a)
instance Traversable (ParseExpr s) where sequence = coerceSequence ParseExpr

type SourceExpr = ParseExpr String (String,Maybe (NameExpr GlobalID))

pattern PE e = ParseExpr (Compose e)
pattern PESym r s = ParseExpr (Compose (r,Pure s))
pattern PEApp r f x = ParseExpr (Compose (r,Join (Compose (Apply f x))))
pattern PELam r s e = ParseExpr (Compose (r,Join (Compose (Lambda s e))))

impossible = error "The impossible has happened"

instance Semantic (ParseExpr s a) s a where
  semNode = iso f g
    where f (PESym _ a) = SemSymbol a
          f (PELam _ s e) = SemAbstract s (PE e)
          f (PEApp _ f x) = SemApply (PE f) (PE x)
          f _ = impossible
          g (SemSymbol a)                          = PESym zero a
          g (SemApply (PE f@(r,_)) (PE x@(r',_)))  = PEApp (r+r') f x
          g (SemApply _ _)                         = impossible
          g (SemAbstract s (PE e@(r,_)))           = PELam r s e
          g (SemAbstract _ _)                      = impossible

newtype OpStream = OpStream [(Char,(Char,Int,Int,Int))]
                   deriving (Semigroup,Monoid)
instance Stream Char OpStream where
  cons c (OpStream l) = OpStream ((c,('\0',0,0,0)):(l & t'head.l'2.l'1 %- c))
  uncons (OpStream []) = Nothing
  uncons (OpStream ((c,_):l)) = Just (c,OpStream l)
instance ParseStream Char OpStream
mkStream :: Stream Char s => s -> OpStream
mkStream = OpStream . mk ('\0',0,0,0)
  where mk (p,n,ln,cl) s = case uncons s of
          Just ('\n',s') -> ('\n',(p,n,ln,cl)):mk ('\n',n+1,ln+1,0) s'
          Just (c,s') -> (c,(p,n,ln,cl)):mk (c,n+1,ln,cl+1) s'
          Nothing -> []

type OpMap_Val = ((Int,Bool),String)
type OpMap = Cofree (Map Char) (Maybe OpMap_Val)
type OpParser m = ParserT OpStream (RWST Void [Warning] (Int,OpMap,(Map String (NameExpr GlobalID),Library)) m)
type Spaces = forall s m. (Monad m,ParseStream Char s) => ParserT s m ()

instance Lens1 a a (Cofree f a) (Cofree f a) where
  l'1 k (Step x f) = k x <&> \x' -> Step x' f
instance Lens2 (f (Cofree f a)) (f (Cofree f a)) (Cofree f a) (Cofree f a) where
  l'2 k (Step x f) = k f <&> Step x

instance Semigroup OpMap where
  Step x y + Step x' y' = Step (x+x') (y*+y')
instance Monoid OpMap where
  zero = Step zero zero
instance DataMap OpMap String OpMap_Val where
  at [] = l'1
  at (c:cs) = l'2.mat c.at cs

data Warning = Warning (Int,Int) String
             deriving (Show,Typeable)
data CurlyParserException = CurlyParserException (Maybe String) [Warning]
                          deriving (Show,Typeable)
instance Exception CurlyParserException
instance Show (Pretty CurlyParserException) where
  show (Pretty (CurlyParserException s ws)) =
    intercalate "\n" [format "  Warning #%d:%s" (i :: Int) (showWarning s w) | (i,w) <- zip [1..] ws]

showWarning :: Maybe String -> Warning -> String
showWarning f (Warning (l,c) s) = format "%s%d:%d: %s" (c'string $ maybe "" (format "%s: ") f) l c s
warn :: Monad m => String -> OpParser m ()
warn s = currentPos >>= \(_,l,c) -> tell [Warning (l,c) s]
expected :: Monad m => String -> OpParser m a -> OpParser m a
expected x p = p <+? (warn (format "expected %s" x) >> zero)
opKeyword :: Monad m => String -> OpParser m ()
opKeyword s = expected (format "opKeyword '%s'" s) (several s)
guardWarn :: Monad m => String -> Bool -> OpParser m ()
guardWarn msg p = if p then unit else (warn msg >> zero)

l'library = l'3.l'2
l'typeMap = l'3.l'1

parseCurly :: (ParseStream Char s,Monad m) => s -> OpParser m a -> m ([Warning]:+:a)
parseCurly s p = (deduce p^..mapping i'RWST.stateT) (mkStream s) zero <&> \((_,ma),_,ws) -> case ma of
  Just (a,_) -> Right a
  Nothing -> Left ws

mkRange :: SourcePos -> SourcePos -> SourceRange
mkRange p p' = SourceRange Nothing p p'

mkLet (Left (s,v)) = maybe id (flip mkApply) v . mkAbstract s
mkLet (Right t) = \e -> foldl1' mkApply (t+[e])

space, spc, hspc, nbsp, nbhsp, hspace :: Spaces
space = hspace + (eol >> skipMany' ("#" >> skipMany' (satisfy (/='\n')) >> eol))
hspace = void $ satisfy (\c -> c==' ' || c=='\t')
spc = skipMany' space
hspc = skipMany' hspace
nbsp = skipMany1' space
nbhsp = skipMany1' hspace

floating = between spc spc
swaying = between hspc hspc
wrapRound = between "(" (expected ")" ")") . floating
wrapCurly = between "{" (expected "}" "}") . floating

previousChar = remaining <&> \(OpStream s) -> case s of
  [] -> '\0'
  ((_,(p,_,_,_)):_) -> p
currentPos = remaining <&> \(OpStream s) -> case s of
  [] -> (0,0,0)
  ((_,(_,n,l,c)):_) -> (n,l,c)

name = do
  pr <- previousChar
  guard (not (isLetter pr)) >> many1' (satisfy isLetter <+? qChar)
    <+? guard (not (isOperator pr)) >> many1' (satisfy isOperator <+? qChar)
  where qChar = single '\\' >> token
          
isOperator c = not (elem c (c'set "{(_\"')} \t\n\\") || isLetter c)
isLetter c = isAlpha c || inside '0' '9' c || c=='\''
edgeName t = if t then "_" else ""
mkSymName l s r isR = edgeName l + intercalate "_" s + edgeName r + edgeName isR
symEdge :: Monad m => OpParser m Bool
symEdge = option' False (True<$"_")

varName :: Monad m => OpParser m String
varName = liftA4 mkSymName symEdge (name`sepBy1'`"_") symEdge symEdge >>= \n ->
  if isKeyIn (last n) (c'set ":=")
  then init n <$ runStreamState (modify (cons (last n))) <*= guard . nonempty
  else return n

mkSymIn :: Semantic e i (String,Maybe (NameExpr GlobalID)) => Map String (NameExpr GlobalID) -> String -> e
mkSymIn m = \n -> mkSymbol (n,lookup n m)

tom, expr, accessorExpr :: Monad m => Spaces -> OpParser m SourceExpr
expr sp = foldl1' mkApply<$>sepBy1' (tom sp) (skipMany1' sp) 
accessorExpr sp = expr sp <*= \e -> defAccessors (map fst (toList e))

tom sp = do
  Step _ opmap <- lift (getl l'2)
  typeMap <- lift (getl l'typeMap)
  let param m c = case m^.at c of Just tl -> return tl ; _ -> zero
      tokParam m = multi <+? (param m =<< (token <*= guard . (/='_')))
        where multi = case m^.at ' ' of
                Just tl -> tl <$ nbsp
                _ -> zero

      operation _sp p = opSuf
        where opSuf = do
                pref <- opPref
                case opmap^.at '_' of
                  Just tl -> flip fix pref $ \mkSuf (d,e) ->
                    (skipMany' _sp >> suffix (liftA2 (&&) p (<=d)) (e:) tl >>= mkSuf)
                    <+? return (d,e)
                  _ -> return pref
              opPref = (tokParam opmap >>= suffix (>=0) id) <+? map ((maxBound :: Int,) . mkSymbol . Just) atom
                       <+? (maxBound,mkSymbol Nothing)<$"_"

      mkOp n = foldl' mkApply (mkSymbol (Just (mkSymIn typeMap n)))
      suffix p mod = suff
        where
          filterM cmp = map (\x -> x <*= guard . cmp . fst . fst)
          suff (Step (Just x@((d,_),_)) m) =
            suffM (map (filterM (>= d)) m) + suffO x + suffM (map (filterM (< d)) m)
          suff (Step Nothing m) = suffM m
          suffM m = (tokParam m >>= suff) + do
            tl <- param m '_'
            guard (any (maybe False (p . fst . fst)) tl)
            let exprSuf tl = between spc spc (operation space (>=0))
                             >>= \(_,e) -> suffix p (mod . (e:)) tl
            case tl :: OpMap of
              Step (Just ((d,isR),n)) m' | p d ->
                foldr1 (<+?) [ exprSuf (filterM (> d) tl)
                             , spc >> operation sp (if isR then (>=d) else (>d))
                               <&> \(d',e) -> (min d d',mkOp n (mod [e]))
                             , exprSuf (filterM (< d) tl) ]
              _ -> exprSuf tl
          suffO ((d,_),n) | p d = return (d,mkOp n (mod []))
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
       =<< wrapCurly (expected "lambda-expression" lambda) <+? wrapRound (expr space) <+? (expected "symbol" close)
  where
    close = liftA2 (&) (liftA2 mkSymIn (lift $ getl l'typeMap) name <+? string '"' <+? string '\'')
            $ option' id $ wrapRound $ do
      sepBy1' (tom space) nbsp <&> \args e -> foldl' mkApply e args
    withPostfix s = foldl' (\e n -> mkApply (mkSymbol ('.':n,Nothing)) e) s
                    <$> many' (single '.' >> many1' letter)
    string c = between (single c) (single c) $ mkConcat . g . foldr f ("",[]) <$> many' stringExpr
      where stringExpr = map Left (single '$' >> wrapCurly (expr space))
                         <+? map Right (single '\\' >> unquote<$>token <+? satisfy (/=c))
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
            g (s,es) = mkSymbol (c:(s+[c]),Nothing):es
    
    lambda = do
      old <- lift get
      args <- fold <$> sepBy1' lambdaArg nbsp
      _ <- floating ":"
      e <- expr space
      lift (put old)
      return $ foldr mkLet e args

lambdaArg :: Monad m => OpParser m [(String, Maybe SourceExpr):+:[SourceExpr]]
lambdaArg = letBinding + funPrefix + do
  n <- (varName <*= register) <+? ("_"<$"_")
  tm <- lift (getl (l'typeMap))
  map (Left (n,Nothing):) $ option [] $
    map (Right [mkSymIn tm n]:) $ wrapRound (map fold (sepBy' lambdaArg nbsp))
  where letBinding = wrapCurly $ expected "let-binding" $ typeBinding <+? do
          n <- varName
          old <- lift get
          args <- fold <$> many' (nbsp >> lambdaArg)
          _ <- floating (opKeyword "=")
          e <- expr space
          lift (put old)
          register n
          return [Left (n,Just (foldr mkLet e args))]
        typeBinding = do
          ((_,_),(ctor,ctt),(dtor,dtt)) <- typeDecl
          lift $ l'typeMap =~ insert ctor (typeExpr ctt) . insert dtor (typeExpr dtt)
          register ctor ; register dtor
          return []
        funPrefix = wrapRound $ pure . Right<$>sepBy1' (tom space) nbsp

typeExpr :: Type GlobalID -> NameExpr GlobalID
typeExpr t = mkAbstract (pureIdent "#0") (mkSymbol (pureIdent "#0",Pure (Argument 0))) & from i'NameNode.t'Join.annType.l'1 %- t

curlyFile :: (Monad m, ?mountain :: Mountain) => OpParser m Library
curlyFile = do
  modFile + symFile
  when (envLogLevel >= Debug) (mtrace "Finished parsing")
  lift (getl l'library)
  where modFile = do
          "#!/lib/module!#" <+? "module"
          syn <- hspc *> synopsis <* (eol+eoi)
          lift (l'library.metadata =~ syn)
          skipMany' (curlyLine <+> (hspc >> eol))
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
          e <- floating (expr space)
          post <- currentPos
          lift $ l'library =~ compose [
            addImport (fold mods'),
            defSymbol "value" (mkRange pre post) Nothing False e,
            setExports (Pure "value")]

raw :: (ParseStream Char s,MonadParser s m p) => String -> p ()
raw = several

curlyLine :: (Monad m, ?mountain :: Mountain) => OpParser m ()
curlyLine = swaying (foldr1 (<+?) [defLine,descLine,typeLine,classLine,comment,impLine,expLine,transLine,setLine,metaLine])
             <* expected "end of line" (eol+eoi)
             >>= \f -> lift $ l'library =~ f
  where withPlural p = p >> option () "s"
        impLine = withPlural "import" >> nbsp >> do
          mods <- sepBy1' modTree nbhsp
          resolved <- fold<$>traverse resolve mods
          guardWarn (format "Nothing to import for '%s' in the current context" (show mods)) (nonempty resolved)
          let newinsts = c'set $ fromKList $ fold [toList (zipWith const ?mountain mod) | mod <- mods]
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
          e <- expr hspace
          post <- currentPos
          lift (put old)
          register sym
          defAccessors (sym:map fst (toList e))
          return $ \l -> case l^.symbols.at sym of
            Just lf | lf^.leafIsMethod ->
                      let e' = optExprIn l (foldr mkLet e args)
                          t' = lf^.leafType
                               + mapTypePathsMonotonic (Just . warp (l'1.t'ImplicitRoot) (+1)) (exprType e'^.l'1)
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
          ((pre,post),(ctor,ctt),(dtor,dtt)) <- typeDecl
          let rng = mkRange pre post
          register ctor; register dtor
          return (defTypeSym ctor False rng ctt (expr_constructor ctt)
                  . defTypeSym dtor False rng dtt (expr_destructor dtt))
        classLine = "family" >> do
          cl <- nbsp >> varName
          indices <- many' $ between "[" "]" $ sepBy1' varName ","
          args <- many' (nbsp >> varName <*= guard . (/=":"))
          _ <- nbsp >> opKeyword ":"
          (pre,tp,post) <- nbsp >> typeSum
          defClass cl args indices (mkRange pre post) tp
        comment = id <$ raw "#" <* skipMany' (satisfy (/='\n'))

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
      mod <- defClass ac ["a","b"] [["a"]] NoRange $ \l ->
        l'1 $^ exprType $ exprIn l (e :: SourceExpr)
      lift (l'library =~ mod)

defTypeSym n isM rng tp e = symbols.at n.l'Just undefLeaf %~
                            set leafVal (set (t'exprType.l'1) tp (_rawNameExpr e))
                            . set leafPos rng
                            . set leafType tp . set leafIsMethod isM
defRigidSymbols args = compose [defTypeSym a False NoRange (rigidTypeFun a) expr_identity
                               | a <- args]

typeSum :: Monad m => OpParser m (SourcePos,Library -> Type GlobalID,SourcePos)
typeSum = do
  let typeNode = (fill Nothing delim <+? map Just (tom hspace)) >>= maybe zero pure
      delim = between hspc nbsp ("and"<+?"*"<+?"&"<+?"×")
  pre <- currentPos
  exprs <- sepBy1' (foldl1' mkApply <$> sepBy1' typeNode nbhsp) delim
  post <- currentPos
  return (pre,(\l -> foldl1' (+) [exprType (exprIn l e)^.l'1 | e <- exprs]),post)
typeDecl = "type" >> nbsp >> do
  mctor <- option' Nothing $ map Just $ do
    varName <* nbsp <* opKeyword ":" <* nbsp
  tname <- varName
  cargs <- many' (nbsp *> varName <*= guard . (/="="))
  let ctor = fromMaybe tname mctor
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
  return ((pre,post),(ctor,tpc),(dtor,tpd))

(<##>) :: Lens s t a a -> Lens s' t' a b -> Lens (s,s') (t,t') a b
l1 <##> l2 = lens (liftA2 (,) (by l1) (by l2)) (\a (t,t') -> set l2 t' (set l1 t a))

register :: Monad m => String -> OpParser m ()
register n = when (elem '_' n && n/="_") $ lift (l'1<##>l'2 =~ \(d,m) -> (d+1,insert nk ((d,isR),n) m))
  where isR = drop (length n-2) n == "__"
        nk = if isR then init n else n
resolve :: (Monad m, ?mountain :: Mountain) => Module (String, Maybe String) -> OpParser m Context
resolve mod = mod' <$ traverse (register.identName.fst) mod'
  where mod' = zipWith r mod localContext
        r (_,Just n) (GlobalID _ l,v) = (GlobalID n l,v)
        r _ x = x
modTree :: Monad m => OpParser m (Module (String,Maybe String))
modTree = tree <&> \x -> Join (ModDir [x])
  where modName = many1' (satisfy (\c -> not (c`elem`c'set " \t\n{()}.\\")) <+?
                          (raw "\\" >> token))
        tree = sepBy1' modName "." >>= \ (n:ns) -> do
          let modN n e = Join (ModDir [(n,e)])
              modNs e = foldr modN e ns
              ln = last (n:ns)
          map ((n,) . modNs) $ option' (Pure (ln,Nothing)) $ hspc *> do
            Join . ModDir <$> wrapCurly (tree`sepBy'`nbsp)
              <+> Pure . (ln,) . Just <$> wrapRound modName

