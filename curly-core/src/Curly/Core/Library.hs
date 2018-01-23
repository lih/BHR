{-# LANGUAGE TypeFamilies, StandaloneDeriving #-}
module Curly.Core.Library(
  -- * Modules
  -- ** Nodes 
  ModDir(..),Module,Mountain,Context,context,localContext,
  atM,atMs,fromPList,
  -- ** Leaves
  ModLeaf,SourcePos,SourceRange(..),
  undefLeaf,leafVal,leafDoc,leafPos,leafType,leafIsMethod,
  -- * Libraries
  GlobalID(..),
  LibraryID(..),isLibData,
  Metadata(..),Library,metadata,imports,exports,symbols,implicits,
  addImport,addExport,setExports,defSymbol,libSymbol,
  exprIn,optExprIn,builtinsLib,
  -- ** Documentation
  LeafExpr,DocNode(..),Documentation,docNodeAttrs,docNodeSubs,descSymbol,docAtom,docLine,mkDoc,
  -- * Files
  FileLibrary,flLibrary,flID,flBytes,flFromSource,flSource,
  rawLibrary,fileLibrary,  
  -- * Repositories
  Template,defaultTemplate,showTemplate,
  RepoConfig(..),repoConfig,Repository(..),repositories,findLib,availableLibs, 
  -- * Miscellaneous
  Compressed(..),envVar,curlyCacheDir,noCurlySuf
  ) where

import Crypto.Hash.SHA256
import Curly.Core
import Curly.Core.Documentation
import Curly.Core.Annotated
import Data.IORef
import Language.Format
import IO.Network.Socket
import Network.Socket(AddrInfo)
import System.Process (runInteractiveProcess)
import Control.Concurrent (forkIO)
import IO.Filesystem
import IO.Time (Seconds, currentTime)
import Codec.Compression.Zlib (compress,decompress)
import GHC.Conc (par)
import Control.DeepSeq

curlyLibVersion :: Int
curlyLibVersion = 9

newtype Compressed a = Compressed { unCompressed :: a }
                     deriving (Show,Eq,Ord)
instance Serializable a => Serializable (Compressed a) where
  encode (Compressed a) = encode (compress (serialize a))
instance Format a => Format (Compressed a) where
  datum = (datum <&> decompress) >*> (Compressed <$> datum)

newtype Chunked a = Chunked { getChunked :: a }
instance Serializable a => Serializable (Chunked a) where
  encode (Chunked a) = encode (serialize a)
instance Format a => Format (Chunked a) where
  datum = datum <&> \x -> maybe (error "Invalid chunked value") Chunked (matches Just (datum <* (guard . (==zero) =<< remaining)) x)

newtype ModDir s a = ModDir [(s,a)]
                      deriving (Semigroup,Monoid,Show)
type Module a = Free (ModDir String) a
instance Documented a => Documented (Module a) where
  document (Join (ModDir l)) = docTag' "ul" (map (docTag "li" [("class","modVal")] . pure . doc') l)
    where doc' (s,Pure n) | s==pretty n = Pure s
                          | otherwise = document n
          doc' (s,Join (ModDir l)) = docTag' "p"
                                     [docTag "ln" [("class","modName")] [Pure (s+":")]
                                     ,docTag' "ul"  (map (docTag "li" [("class","modVal")] . pure . doc') l)]
  document (Pure s) = document s

instance (Serializable s,Serializable a) => Serializable (ModDir s a) where
  encode = coerceEncode (ModDir . getChunked)
instance (Format s,Format a) => Format (ModDir s a) where
  datum = coerceDatum (ModDir . getChunked)
instance Functor (ModDir s) where map f (ModDir l) = ModDir (l <&> l'2 %~ f)
instance Ord s => SemiApplicative (Zip (ModDir s)) where
  Zip (ModDir fs) <*> Zip (ModDir xs) = Zip (ModDir (fs >>= \(s,f) -> fold (xm^.at s) <&> (s,) . f))
    where xm = c'map $ compose [at s.folded %~ (+[x]) | (s,x) <- xs] zero
instance Foldable (ModDir s) where
  fold (ModDir l) = foldMap snd l
instance Traversable (ModDir s) where
  sequence (ModDir l) = ModDir <$> traverse (\(s,a) -> (s,)<$>a) l

atM :: Eq s => s -> a -> Traversal' (ModDir s a) a
atM s a k (ModDir d) = map ModDir $ for d' $ \(s',a) -> (s',) <$> if s'==s then k a else return a
  where d' | has (at s.traverse) d = d
           | otherwise = (s,a):d
atMs :: Eq s => [s] -> Traversal' (Free (ModDir s) a) (Free (ModDir s) a)
atMs [] k x = k x
atMs (s:ss) k (Join m) = Join<$>(atM s (Join zero).atMs ss) k m
atMs (s:ss) k _ = Join<$>(atM s (Join zero).atMs ss) k zero

fromPList l = compose [atMs p %- v | (p,v) <- l] (Join zero)

newtype LibraryID = LibraryID Chunk
                deriving (Eq,Ord,Generic)
idSize :: Int
idSize = 32

instance Serializable LibraryID where
  encode (LibraryID x) = x^.chunkBuilder
instance Format LibraryID where
  datum = LibraryID<$>getChunk idSize
instance NFData LibraryID
instance Show LibraryID where
  show (LibraryID l) = show (B64Chunk l)
instance Read LibraryID where
  readsPrec _ = readsParser (readable >>= \(B64Chunk c) -> LibraryID c <$ guard (chunkSize c==idSize))
instance Documented LibraryID where document l = Pure (show l)

registerLib :: FileLibrary -> FileLibrary
registerLib l = by thunk $ do
  let i = l^.flID
  logLine Debug $ format "Registering library %s" (show i)
  i`seq`modifyIORef libraryCache (insert i (Just l))
  return l
rawLibrary :: Bool -> Library -> Bytes -> Maybe String -> FileLibrary
rawLibrary new l b src = registerLib (FileLibrary l b (LibraryID (hashlazy b)) new src)
fileLibrary :: Library -> Maybe String -> FileLibrary 
fileLibrary l = rawLibrary True l (serialize l)
isLibData :: LibraryID -> Bytes -> Bool
isLibData (LibraryID i) bs = hashlazy bs==i 

data ModLeaf s a = ModLeaf {
  _leafDoc :: Documentation,
  _leafPos :: SourceRange,
  _leafType :: Type s,
  _leafIsMethod :: Bool,
  _leafVal :: a
  }
                 deriving Generic
instance Functor (ModLeaf s) where
  map = warp leafVal 
instance Foldable (ModLeaf s) where fold l = l^.leafVal
instance Traversable (ModLeaf s) where sequence l = leafVal id l
instance (Identifier s,Serializable s,Serializable a) => Serializable (ModLeaf s a) where
  encode (ModLeaf a b c d e) = encode (Chunked a)+encode b+encode (Chunked c)+encode d+encode (Chunked e)
instance (Identifier s,Format s,Format a) => Format (ModLeaf s a) where
  datum = (\(Chunked a) b (Chunked c) d (Chunked e) -> ModLeaf a b c d e)
          <$>datum<*>datum<*>datum<*>datum<*>datum
instance (Identifier s,Identifier s') => HasIdents s s' (ModLeaf s a) (ModLeaf s' a) where
  ff'idents = leafType.ff'idents

type SourcePos = (Int,Int,Int)
data SourceRange = SourceRange (Maybe String) SourcePos SourcePos
                 | NoRange
instance Semigroup SourceRange where
  SourceRange f a b + SourceRange g c d = SourceRange (g+f) (min a c) (max b d)
  NoRange + a = a
  a + NoRange = a
instance Monoid SourceRange where zero = NoRange
instance Serializable SourceRange where
  encode (SourceRange _ b c) = encodeAlt 0 (b,c)
  encode NoRange = encodeAlt 1 ()
instance Format SourceRange where
  datum = datumOf [FormatAlt (uncurry $ SourceRange Nothing),FormatAlt (uncurry0 NoRange)]
  
leafDoc :: Lens' (ModLeaf s a) Documentation
leafDoc = lens _leafDoc (\x y -> x { _leafDoc = y })
leafPos :: Lens' (ModLeaf s a) SourceRange
leafPos = lens _leafPos (\x y -> x { _leafPos = y })
leafIsMethod :: Lens' (ModLeaf s a) Bool
leafIsMethod = lens _leafIsMethod (\x y -> x { _leafIsMethod = y })
leafType :: Lens (Type s) (Type s') (ModLeaf s a) (ModLeaf s' a)
leafType = lens _leafType (\x y -> x { _leafType = y })
leafVal :: Lens a b (ModLeaf s a) (ModLeaf s b)
leafVal = lens _leafVal (\x y -> x { _leafVal = y })

type LeafExpr s = ModLeaf s (NameExpr s)

data GlobalID = GlobalID String (Maybe (String,LibraryID))
           deriving (Eq,Ord,Show,Generic)
instance Documented GlobalID where
  document = if envLogLevel>=Verbose
             then \(GlobalID n l) -> Pure (n+showL l)
             else \(GlobalID n _) -> Pure n
    where showL (Just (n,l)) = "["+show l+":"+n+"]"
          showL _ = "[]"
instance Serializable GlobalID
instance Format GlobalID
instance NFData GlobalID
instance Identifier GlobalID where
  pureIdent n = GlobalID n Nothing
  identName (GlobalID n _) = n
type Context = Module (GlobalID,LeafExpr GlobalID)
newtype Metadata = Metadata (Forest (Map String) String)
                 deriving (Semigroup,Monoid,Serializable)
instance Format Metadata where datum = coerceDatum Metadata
instance DataMap Metadata String (Free (Map String) String) where 
  at i = iso (\(Metadata m) -> m) Metadata .at i
instance Show Metadata where
  show (Metadata m) = showM m
    where showM m = format "{%s}" (intercalate " " [format "%s:%s" (show a) (showV v)
                                                   | (a,v) <- m^.ascList])
          showV (Pure s) = show s
          showV (Join m) = showM m
instance Read Metadata where
  readsPrec _ = readsParser (map Metadata brack)
    where val = map Pure readable <+? map Join brack
          brack = fromAList <$> between (single '{') (single '}') (sepBy' assoc (single ' '))
            where assoc = liftA2 (,) readable (single ':' >> val)
instance Documented Metadata where
  document m = Pure (show m)

data Library = Library {
  _metadata :: Metadata,
  _imports :: Context,
  _symbols :: Map String (LeafExpr GlobalID),
  _externalSyms :: Map String GlobalID,
  _implicits :: InstanceMap GlobalID (Maybe LibraryID,LeafExpr GlobalID),
  _exports :: Context
  }

metadata :: Lens' Library (Metadata)
metadata = lens _metadata (\x y -> x { _metadata = y })
imports :: Lens' Library Context
imports = lens _imports (\x y -> x { _imports = y })
symbols :: Lens' Library (Map String (LeafExpr GlobalID))
symbols = lens _symbols (\x y -> x { _symbols = y })
externalSyms :: Lens' Library (Map String GlobalID)
externalSyms = lens _externalSyms (\x y -> x { _externalSyms = y })
implicits :: Lens' Library (InstanceMap GlobalID (Maybe LibraryID,LeafExpr GlobalID))
implicits = lens _implicits (\x y -> x { _implicits = y })
exports :: Lens' Library Context
exports = lens _exports (\x y -> x { _exports = y })

instance Semigroup Library where
  Library syn i s es is e + Library _ i' s' es' is' e' = Library syn (i+i') (s+s') (es+es') (is+is') (e+e')
instance Monoid Library where
  zero = Library (Metadata zero) zero zero zero zero zero
instance Show Library where
  show (Library (Metadata syn) imp sym _ _ _exp) =
    "Library: "+fromMaybe "" (syn^?at "synopsis".t'Just.t'Pure) +"\n"
    + "Imports: \n"
    + indent "  " (pretty (by l'1<$>imp))+"\n"
    + "Exports: \n"
    + indent "  " (pretty (by l'1<$>_exp))+"\n"
    + "Symbols: \n"
    + foldMap showSym (sym^.keyed)
    where showSym (s,lf) = "### "+pretty s+"\n"+indent "# " (pretty (map fst $ c'Expression $ semantic (leafVal$^lf)))+"\n"
cylMagic :: String
cylMagic = "#!/lib/cyl!# "
newtype ParEncode t = ParEncode t
instance (Ord k,Serializable k, Serializable a) => Serializable (ParEncode (Map k a)) where
  encode (ParEncode m) = let l = foldr (\x y -> yb chunkBuilder x`par`x:y) [] [encode x | x <- m^.ascList]
                         in encode (length l) + fold l
instance (Ord k,Format k,Format a) => Format (ParEncode (Map k a)) where
  datum = ParEncode . yb ascList<$>datum
instance Serializable Library where
  encode l = foldMap encode cylMagic
             + let (m,(a,b,c,d,e,f,g)) = l^.scoped.withStrMap
                   syn = fromMaybe "" (a^?at "synopsis".t'Just.t'Pure)
               in foldMap encode (syn+"\n") + encode (curlyLibVersion,Compressed (m,
                                                                                  Chunked (delete "synopsis" a),
                                                                                  Chunked (map Chunked b),
                                                                                  Chunked c,
                                                                                  d,
                                                                                  Chunked e,
                                                                                  f,
                                                                                  g))
instance Format Library where
  datum = do
    traverse_ (\c -> datum >>= guard . (c==)) cylMagic
    syn <- many' (datum <*= guard . (/='\n')) <* (datum >>= guard . (=='\n'))
    datum >>= \(vers,Compressed (m,Chunked a,Chunked b,Chunked c,d,Chunked e,f,g)) -> do
      guard (vers == curlyLibVersion)
      return $ (m,(insert "synopsis" (Pure syn) a,map getChunked b,c,d,e,f,g))^..scoped.withStrMap

type ExprRep s = ModLeaf s (Expression s (s,Maybe (Symbol s)))
type LibRep s = (Metadata,Module s
                ,Map String (ExprRep s)
                ,Map String s
                ,InstanceMap s (ExprRep s)
                ,Set LibraryID
                ,Module s)
scoped :: Iso' Library (LibRep GlobalID)
scoped = iso f g
  where f (Library syn i s es is e) = (syn,map fst i,map2 toExpr s,es,map2 toExpr (filterInsts is),instDeps,map fst e)
          where toSym (s,Pure sym) = (s,Just sym)
                toSym (s,_) = (s,Nothing)
                  
                toExpr = map toSym . c'Expression . semantic
                filterInsts = map snd . warp ascList (\l -> [x | x@(_,(Nothing,_)) <- l])
                instDeps = c'set $ fromKList [k | (Just k,_) <- toList is]
        g (syn,i',s',es,is',isd,e') = Library syn i s es is e
          where symVal (GlobalID _ (Just (s,l))) = fromMaybe (error $ "Couldn't find library "+show l) (findLib l)
                                                   ^.flLibrary.symbols.at s.l'Just undefLeaf
                symVal (GlobalID i Nothing) = s^.at i.l'Just undefLeaf
                fromSym (s,Just sym) = (s,Pure sym)
                fromSym (s,Nothing) = (s,Join (symVal s^.leafVal))
                fromExpr = withType . map (_rawNameExpr . semantic . c'Expression . map fromSym)
                withType s = s & set (leafVal.t'exprType) (s^.leafType)
                i = map (\s -> (s,symVal s)) i'
                e = map (\s -> (s,symVal s)) e'
                s = map fromExpr s'
                is = map ((Nothing,) . fromExpr) is' + fold [fl^.flLibrary.implicits
                                                            | Just fl <- map findLib (keys isd)]

withStrMap :: Iso' (LibRep GlobalID) (Map Int GlobalID,LibRep Int)
withStrMap = iso f g
  where f (n,i,v,ev,iv,ivd,o) = (toMap (commute strs),(n,i',v',ev',iv',ivd,o'))
          where ((_,strs),(i',v',ev',iv',o')) = yb state foo zero
                strId s = id <~ \(sz,m) -> case lookup s m of
                  Just i -> ((sz,m),i)
                  _ -> ((sz+1,insert s sz m),sz)
                nodeId (Lambda s a) = strId s <&> \s' -> Lambda s' a
                nodeId (Apply a b) = return (Apply a b)
                exprId l = map (c'ExprRep c'int) $ (>>= traversel ff'idents strId) $ forl leafVal l $ \e -> do
                  traverseF nodeId e >>= traverse (traversel (l'1.+l'2.t'Just.symIdents) strId)
                foo = do
                  i' <- traverse strId i
                  v' <- traverse exprId v
                  ev' <- traverse strId ev
                  iv' <- traversel ff'idents strId =<< traverse exprId iv
                  o' <- traverse strId o
                  return (i',v',ev',iv' :: InstanceMap Int (ExprRep Int),o')
        c'ExprRep :: Constraint a -> Constraint (ExprRep a)
        c'ExprRep _ = c'_
        c'GlobalID = c'_ :: Constraint GlobalID
        symIdents :: (Identifier s,Identifier s') => FixFold s s' (Symbol s) (Symbol s')
        symIdents = ff'idents
        g (m,(n,i',v',ev',iv',ivd,o')) = (n,map idSym i',v,ev,iv,ivd,map idSym o')
          where idSym :: Int -> GlobalID
                idSym i = fromMaybe (error "Undefined identifier ID") (lookup i m)
                exprSym l = (c'ExprRep c'GlobalID . warp ff'idents idSym
                             . map (mapF node . map (idSym<#>warp (t'Just.symIdents) idSym))) l
                node (Lambda i a) = Lambda (idSym i) a
                node (Apply a b) = Apply a b
                v = map exprSym v'
                iv = (warp ff'idents idSym . map exprSym) iv' :: InstanceMap GlobalID (ExprRep GlobalID)
                ev = map idSym ev'

-- | A library with cached versions of its library ID and serialized self
data FileLibrary = FileLibrary {
  _flLibrary :: Library,
  _flBytes :: Bytes,
  _flID :: LibraryID,
  _flFromSource :: Bool,
  _flSource :: Maybe String
  }
instance Eq FileLibrary where a==b = compare a b==EQ
instance Ord FileLibrary where compare = comparing _flID
flLibrary :: Lens' FileLibrary Library
flLibrary = lens _flLibrary (\x y -> x { _flLibrary = y }) 
flID :: Lens' FileLibrary LibraryID
flID = lens _flID (\x y -> x { _flID = y }) 
flBytes :: Lens' FileLibrary Bytes
flBytes = lens _flBytes (\x y -> x { _flBytes = y })
flFromSource :: Lens' FileLibrary Bool
flFromSource = lens _flFromSource (\x y -> x { _flFromSource = y })
flSource :: Lens' FileLibrary (Maybe String)
flSource = lens _flSource (\x y -> x { _flSource = y })

type Mountain = Module FileLibrary
mapIdents :: (String -> GlobalID -> GlobalID) -> (GlobalID -> GlobalID) -> Context -> Context
mapIdents sw f = mapC "" 
  where mapDE = warp (leafType.ff'idents) f . warp leafVal mapE
        mapE = warp (from i'NameNode) (map (first f)) . warp (t'exprType.ff'idents) f
        mapC _ (Join (ModDir m)) = Join . ModDir $ warp each (\(s,e) -> (s,mapC s e)) m
        mapC s (Pure (i,e)) = Pure (sw s (f i),mapDE e)
context :: Mountain -> Context
context m = m >>= \fl -> mapIdents (\s (GlobalID _ l) -> GlobalID s l) (setId (fl^.flID)) (fl^.flLibrary.exports)
  where setId i (GlobalID n Nothing) = GlobalID n (Just (n,i))
        setId _ x = x
localContext :: (?mountain :: Mountain) => Context
localContext = context ?mountain
                    
undefSym :: NameExpr GlobalID
undefSym = mkSymbol (pureIdent "undefined",Pure (Builtin (builtinType B_Undefined) B_Undefined))
undefLeaf :: LeafExpr GlobalID
undefLeaf = ModLeaf nodoc NoRange zero False undefSym

addImport :: Context -> Library -> Library
addImport imp = warp imports (+imp) . warp symbols (fromAList (map2 snd newSyms)+)
                . warp externalSyms (fromAList (map2 fst newSyms)+)
  where f (i,e) = (identName i,(i,warp leafVal (\e' -> mkSymbol (i,Join e')) e))
        newSyms = map f (toList imp)
resolve :: Library -> Module String -> Context
resolve l e = map go e
  where go n = (fromMaybe (pureIdent n) (l^.externalSyms.at n),
                fromMaybe undefLeaf (l^.symbols.at n))
addExport :: Module String -> Library -> Library
addExport e l = l & exports %~ (+resolve l e)
setExports :: Module String -> Library -> Library
setExports e l = l & exports %- resolve l e
defSymbol :: Semantic e String (String,Maybe (NameExpr GlobalID)) => String -> SourceRange -> Maybe (Type GlobalID) -> Bool -> e -> Library -> Library
defSymbol s r t isM e l = l & symbols.at s.l'Just undefLeaf %~ set leafType tp . set leafVal e' . set leafPos r . set leafIsMethod isM
  where e' = optExprIn l e 
        tp = fromMaybe (exprType e') t

exprIn :: Semantic e String (String,Maybe (NameExpr GlobalID)) => Library -> e -> NameExpr GlobalID
exprIn l e = syntax merge val (pureIdent . fst) (\n -> Pure (Argument n)) (c'Expression $ mapParams pureIdent e)
  where val (s',Nothing) = fromMaybe (Pure (builtin B_Undefined))
                           $ matches Just ((Pure . builtin . B_Number <$> readable
                                            <+? Pure . builtin . B_String <$> readable
                                            <+? (Pure (builtin B_Undefined)<$single '…')
                                            <+? (Pure (builtin B_AddString)<$several "#concat")) <* eoi) (pretty s')
                           + map (Join . by leafVal) (l^.symbols.at s')
        val (_,Just x) = Join x
        builtin b = Builtin (builtinType b) b
        merge (s,_) x = (pureIdent s,x)
optExprIn :: Semantic e String (String,Maybe (NameExpr GlobalID)) => Library -> e -> NameExpr GlobalID
optExprIn l e = optimize (pureIdent . pretty) (solveConstraints (map (\(_,lf) -> (lf^.leafType,lf^.leafVal)) (l^.implicits)) (exprIn l e))

descSymbol :: String -> Documentation -> Library -> Library
descSymbol s d l = l & symbols.at s.l'Just undefLeaf.leafDoc %- d

libSymbol :: Library -> GlobalID -> Maybe (LeafExpr GlobalID)
libSymbol l (GlobalID i Nothing) = l^.symbols.at i
libSymbol _ (GlobalID _ (Just (i,l))) = findLib l >>= \l -> l^.flLibrary.symbols.at i

builtinsLib :: FileLibrary
builtinsLib = let blib = zero
                         & set symbols (fromAList [(foldl' (flip const) ph pt,v) | (ph:pt,(_,v)) <- allBuiltins])
                         . set exports builtinsMod
                         . set metadata (Metadata meta)
              in rawLibrary False blib (serialize blib) Nothing
  where Join meta = fromAList [(["synopsis"],Pure "The Curly Builtin Library")
                              ,(["author","name"],Pure "Marc Coiffier")
                              ,(["author","email"],Pure "marc@coiffier.net")
                              ,(["version"],Pure "0.5")]
        safeLast x [] = x
        safeLast _ (h:t) = safeLast h t
        builtinsMod = fromPList (map2 Pure allBuiltins) 
        allBuiltins = [
          (["undefined"],(pureIdent "undefined",undefLeaf)),
          (["seq"],mkBLeaf "seq" B_Seq seqDoc),
          (["unit"],mkBLeaf "unit" B_Unit unitDoc),
          (["file","open"],mkBLeaf "open" B_Open openDoc),
          (["file","read"],mkBLeaf "read" B_Read readDoc),
          (["file","write"],mkBLeaf "write" B_Write writeDoc),
          (["file","close"],mkBLeaf "close" B_Close closeDoc),
          (["file","stdin"],mkBLeaf "stdin" (B_FileDesc 0) stdinDoc),
          (["file","stdout"],mkBLeaf "stdout" (B_FileDesc 1) stdoutDoc),
          (["arithmetic","addInt"],mkBLeaf "addInt" B_AddInt addIntDoc),
          (["arithmetic","subInt"],mkBLeaf "subInt" B_SubInt subIntDoc),
          (["arithmetic","mulInt"],mkBLeaf "mulInt" B_MulInt mulIntDoc),
          (["arithmetic","divInt"],mkBLeaf "divInt" B_DivInt divIntDoc),
          (["arithmetic","cmpInt_lt"],mkBLeaf "cmpInt_lt" B_CmpInt_LT cmpInt_ltDoc),
          (["arithmetic","cmpInt_eq"],mkBLeaf "cmpInt_eq" B_CmpInt_EQ cmpInt_eqDoc),
          (["string","addString"],mkBLeaf "addString" B_AddString addStringDoc),
          (["string","stringLength"],mkBLeaf "stringLength" B_StringLength stringLengthDoc),
          (["string","showInt"],mkBLeaf "showInt" B_ShowInt showIntDoc),
          (["array","mkArray"],mkBLeaf "mkArray" B_MkArray mkArrayDoc),
          (["array","arrayLength"],mkBLeaf "arrayLength" B_ArrayLength arrayLengthDoc),
          (["array","arrayAt"],mkBLeaf "arrayAt" B_ArrayAt arrayAtDoc),
          (["array","arraySet"],mkBLeaf "arraySet" B_ArraySet arraySetDoc),
          (["syntax","mkSyntaxNode"],mkBLeaf "mkSyntaxNode" B_SyntaxNode mkSyntaxNodeDoc),
          (["syntax","mkSyntaxSym"],mkBLeaf "mkSyntaxSym" B_SyntaxSym mkSyntaxSymDoc),
          (["syntax","mkSyntaxExpr"],mkBLeaf "mkSyntaxExpr" B_SyntaxExpr mkSyntaxExprDoc),
          (["syntax","syntaxInd"],mkBLeaf "syntaxInd" B_SyntaxInd syntaxIndDoc),
          (["syntax","mkExprLambda"],mkBLeaf "mkExprLambda" B_ExprLambda mkExprLambdaDoc),
          (["syntax","mkExprApply"],mkBLeaf "mkExprApply" B_ExprApply mkExprApplyDoc),
          (["syntax","mkExprSym"],mkBLeaf "mkExprSym" B_ExprSym mkExprSymDoc),
          (["syntax","exprInd"],mkBLeaf "exprInd" B_ExprInd exprIndDoc)
          ]
            where mkBLeaf n b d = (pureIdent n,undefLeaf & leafVal %- mkSymbol (pureIdent n,Pure (Builtin (builtinType b) b)) & leafDoc %- mkDoc d)
                  seqDoc = unlines [
                    "{section {title Sequence Expressions}",
                    "  {p {em Usage:} seq x y}",
                    "  {p Evaluates its two arguments in order.}}"
                    ]
                  unitDoc = unlines [
                    "{section {title The Unit value}",
                    "  {p Useful as a placeholder where values are irrelevant}}"
                    ]
                  openDoc = unlines [
                    "{section {title Open File}",
                    "{p {em Usage:} open name \\{file: ...\\}}",
                    "{p Opens a file and passes the file descriptor to the continuation in the second argument}}"
                    ]
                  readDoc = unlines [
                    "{section {title Read From File}",
                    "{p {em Usage:} read file n \\{str: ...\\}}",
                    "{p Reads a number of bytes from the given file and passes the resulting string to the continuation.}}"
                    ]
                  writeDoc = unlines [
                    "{section {title Write To File}",
                    "{p {em Usage:} write file str}",
                    "{p Writes the given bytes to the given file.}}"
                    ]
                  closeDoc = unlines [
                    "{section {title Close File}",
                    "{p {em Usage:} close file}",
                    "{p Closes a file.}}"
                    ]
                  stdoutDoc = unlines [
                    "{section {title The Standard Output Descriptor}",
                    "  {p You can pass this to the 'write' function to",
                    "  print a message to the screen}}"
                    ]
                  stdinDoc = unlines [
                    "{section {title The Standard Input Descriptor}",
                    "  {p You can pass this to the 'read' function to",
                    "  retrieve user-written text.}}"
                    ]
                  addIntDoc = unlines [
                    "{section {title Add Integers}",
                    "{p {em Usage:} addInt a b}",
                    "{p Adds two integers.}}"
                    ]
                  subIntDoc = unlines [
                    "{section {title Subtract Integers}",
                    "{p {em Usage:} subInt a b}",
                    "{p Subtracts two integers.}}"
                    ]
                  mulIntDoc = unlines [
                    "{section {title Multiply Integers}",
                    "{p {em Usage:} mulInt a b}",
                    "{p Multiplies two integers.}}"
                    ]
                  divIntDoc = unlines [
                    "{section {title Divide Integers}",
                    "{p {em Usage:} divInt a b}",
                    "{p Divides two integers.}}"
                    ]
                  cmpInt_ltDoc = unlines [
                    "{section {title Compare Integers (lower than)}",
                    "{p {em Usage:} cmpInt n m x y}",
                    "{p Returns x when n<m, and y otherwise.}}"
                    ]
                  cmpInt_eqDoc = unlines [
                    "{section {title Compare Integers (equality)}",
                    "{p {em Usage:} cmpInt n m x y}",
                    "{p Returns x when n=m, and y otherwise.}}"
                    ]
                  addStringDoc = unlines [
                    "{section {title Add Strings}",
                    "{p {em Usage:} addString a b}",
                    "{p Adds two strings.}}"
                    ]
                  stringLengthDoc = unlines [
                    "{section {title String Length}",
                    "{p {em Usage:} stringLength s}",
                    "{p Gets the length of a string.}}"
                    ]
                  showIntDoc = "{section {title Show Number} Produces a string representation of its argument}"
                  mkArrayDoc = "{section {title Make Array} {p Usage: mkArray n {i: ...}} {p Creates an array of size n, populated by calling the given function on every index from 0 to n-1}}"
                  arrayLengthDoc = "{section {title Get Array Length} {p Gets the length of an array.}}"
                  arrayAtDoc = "{section {title Get Array Element} {p Usage: arrayAt arr i} {p Gets the element at index i in the array arr}}"
                  arraySetDoc = "{section {title Set Array Element} {p Usage: arraySet arr i x k} {p Sets the element at index i, then evaluate k}}"
                  mkSyntaxNodeDoc = ""
                  mkSyntaxSymDoc = ""
                  mkSyntaxExprDoc = ""
                  syntaxIndDoc = ""
                  mkExprLambdaDoc = ""
                  mkExprApplyDoc = ""
                  mkExprSymDoc = ""
                  exprIndDoc = ""

data RepoConfig = RepoConfig { repoProtoRoots :: [String] }
data Repository = CustomRepo String (RepoConfig -> IO (Expires [(LibraryID,Metadata)])) (RepoConfig -> LibraryID -> IO Bytes)
                | CurlyRepo String PortNumber

instance Eq Repository where a == b = compare a b == EQ
instance Ord Repository where
  compare (CustomRepo a _ _) (CustomRepo b _ _) = compare a b
  compare (CustomRepo _ _ _) _ = LT
  compare (CurlyRepo s p) (CurlyRepo s' p') = compare (s,p) (s',p')
  compare (CurlyRepo _ _) _ = GT
instance Show Repository where
  show (CustomRepo b _ _) = b
  show (CurlyRepo h p) = "curly://"+h+":"+show p
instance Read Repository where
  readsPrec _ = map2 swap (repository^..parser)
    where repository =
            liftA2 (CurlyRepo . mkHost) ((single '@' <+? several "curly://") *> many1' (noneOf ": \t\n\n"))
            (option 25465 (single ':' *> map fromInteger readable))
            <+? liftA2 mkRepo (customArg ":") (many1' (oneOf ": ") >> customArg "")
            where customArg x = many1' (noneOf (x+", \t\n\\") <+? (single '\\' *> token))
                  mkHost "_" = "127.0.0.1"
                  mkHost h = h
                  mkRepo proto path = CustomRepo (proto+":"+path) getLs getL
                    where getL conf l = do
                            (_,out,_,_) <- flip fix (repoProtoRoots conf) $ \again dirs -> case dirs of
                              (dir:dirs) -> try (again dirs) $ runInteractiveProcess (dir</>proto) ([path,show l]) Nothing Nothing
                              [] -> undefined
                            readHBytes out
                          lib = liftA2 (,) readable (many' space >> readable)
                          getLs conf = do
                            (_,out,_,_) <- flip fix (repoProtoRoots conf) $ \again dirs -> case dirs of
                              (dir:dirs) -> try (again dirs) $ runInteractiveProcess (dir</>proto) [path] Nothing Nothing
                              [] -> undefined
                            (0,) . foldMap (matches pure lib) . lines <$> readHString out
                    

repoConfig :: IORef RepoConfig
repoConfig = newIORef (RepoConfig [])^.thunk
repositories :: IORef (Set Repository)
repositories = newIORef (fromKList envRepositories)^.thunk
  where envRepositories = fromMaybe [] $ matches Just cpath $ case envVar "" "CURLY_PATH" of
          "" -> fileRepos
          x -> x
          where cpath = skipMany' sp >> sepBy' readable (skipMany1' sp)
                sp = oneOf " \t\n,"
                fileRepos = liftA2 (\x y -> x+" "+y) (fr (curlyUserDir+"/repositories")) (fr "/etc/curly/repositories")^.thunk
                fr n = trylog (return "") (readString n)
type Expires t = (Seconds,t)
listCache :: IORef (Map Repository (Expires [(LibraryID,Metadata)]))
listCache = newIORef zero^.thunk
libraryCache :: IORef (Map LibraryID (Maybe FileLibrary))
libraryCache = newIORef zero^.thunk

nslookup :: String -> PortNumber -> Maybe AddrInfo
nslookup = curry $ cached $ \(s,p) -> convert $ thunk $^ getAddrInfo Nothing (Just s) (Just (show p))

type Template = Documentation
defaultTemplate = docTag' "$*" []

showTemplate :: Metadata -> Template -> Maybe String
showTemplate _ (Pure x) = return x
showTemplate (Metadata m) (Join (DocTag "$*" [] x)) = do
  v <- traverse (showTemplate (Metadata m)) x
  show . Metadata <$> (Join m^?at v.t'Just.t'Join)
showTemplate (Metadata m) (Join (DocTag "$" [] x)) = do
  (vh:vt) <- traverse (showTemplate (Metadata m)) x
  m^?at vh.t'Just.at vt.t'Just.t'Pure
showTemplate m (Join (DocTag "if" [] (p:vs))) = showTemplate m p >> map fold (traverse (showTemplate m) vs)
showTemplate m (Join (DocTag "not" [] vs)) = maybe (return "") (const zero) (traverse_ (showTemplate m) vs)
showTemplate m (Join (DocTag "and" [] vs)) = last . ("":) <$> traverse (showTemplate m) vs
showTemplate m (Join (DocTag "or" [] vs)) = foldMap (showTemplate m) vs
showTemplate m (Join (DocTag cmd [] [a,b]))
  | cmd`elem`["<",">","<=",">="] = do
      [a',b'] <- traverse (showTemplate m) [a,b]
      let valList = many' (map Left number <+? map Right (many1' (satisfy (not . inRange '0' '9'))))
          toOp "<" = (<)
          toOp ">" = (>)
          toOp "<=" = (<=)
          toOp ">=" = (>=)
          toOp _ = undefined
      [a'',b''] <- traverse (matches Just valList) [a',b']
      guard (toOp cmd a'' b'')
      return a'
  | cmd == "=" = do
      x <- showTemplate m a
      p <- showTemplate m b
      matches Just (wildcards p) x
      return x
showTemplate m (Join x) = fold <$> traverse (showTemplate m) x

wildcards "*" = unit
wildcards ('*':'*':t) = wildcards ('*':t)
wildcards ('*':t@(c:_)) = do
  skipMany1' (satisfy (/=c))`sepBy`many1' (single c)
  wildcards t
wildcards (c:t) = single c >> wildcards t
wildcards [] = eoi

cacheName :: LibraryID -> String
cacheName l = cacheFileName curlyCacheDir (show l) "cyl"

getRepoLib :: LibraryID -> Repository -> IO (Maybe FileLibrary)
getRepoLib l r = do
  logLine Verbose $ format "Looking up library %s from repository %s" (show l) (show r)
  res <- (map checkHash . trylog (return zero) . findL) r
  case res of
    Just (_,b) -> void $ forkIO $ do
      let f = cacheName l
      createFileDirectory f
      writeBytes f b
      modifyPermissions f (set (each.executePerm) True)
    _ -> unit
  return (res <&> \(f,b) -> FileLibrary f b l False Nothing)
  where findL (CurlyRepo h p) = case nslookup h p of
          Nothing -> return zero
          Just a -> do
            logLine Verbose $ format "Requesting library %s from curly://%s:%s" (show l) h (show p)
            conn <- connect a
            writeHString conn (show l)
            readHBytes conn
        findL (CustomRepo _ _ getL) = readIORef repoConfig >>= \conf -> getL conf l
        checkHash b | isLibData l b = (,b) <$> matches Just datum b
                    | otherwise = Nothing
        a </> b = a+"/"+b

listRepository :: Repository -> IO [(LibraryID,Metadata)]
listRepository r = do
  now <- currentTime
  (lookup r<$>readIORef listCache) >>= \x -> case x of
    Just (exp,ls) | exp >= now -> return ls
    _ -> do
      ret <- trylog (return zero) (list r)
      runAtomic listCache (at r =- Just ret)
      runAtomic libraryCache $ for_ (snd ret) $ \(l,_) -> at l =~ \p -> Just $ case p of
        Just x@(Just _) -> x
        Nothing -> (readCachedLibrary l^.thunk)+(getRepoLib l r^.thunk)
        Just Nothing -> getRepoLib l r^.thunk
      return (snd ret)
  where list (CurlyRepo h p) = do
          conn <- connectTo h p
          now <- currentTime
          map ((now+15,) . fromMaybe zero) $ runConnection Just True conn $
            liftIO (?write (foldMap encode "libraries"^..bytesBuilder)) >> receive
        list (CustomRepo _ getLs _) = readIORef repoConfig >>= getLs

availableLibs :: IO [(LibraryID,Metadata)]
availableLibs = do
  repos <- readIORef repositories
  fold <$> traverse listRepository (toList repos)
  
readCachedLibrary l = do
  b <- trylog (return zero) $ readBytes (cacheName l)
  return $ do
    guard (isLibData l b)
    f <- matches Just datum b
    return (registerLib $ FileLibrary f b l False Nothing)

registerBuiltinsLib :: ()
registerBuiltinsLib = by thunk $ void (yb thunk builtinsLib)
findLibrary :: Set Repository -> LibraryID -> IO (Maybe FileLibrary)
findLibrary rs l = l`seq`registerBuiltinsLib`seq`fix $ \it -> do
  c <- readIORef libraryCache
  case lookup l c of
    Just (Just l) -> return (Just l)
    _ -> do
      cl <- readCachedLibrary l
      case cl of
        Just _ -> return cl
        _ -> do
          ls <- readIORef listCache
          let rs' = rs - keysSet ls
          logLine Verbose $ format "Missing repositories when searching for library %s: [%s] (cached [%s])"
            (show l) (intercalate "," (map show rs')) (intercalate "," (map show (keysSet ls)))
          if empty rs' then return Nothing
            else do
            for_ rs' (void . listRepository)
            it

findLib :: LibraryID -> Maybe FileLibrary
findLib l = by thunk (readIORef repositories >>= \rs -> findLibrary rs l)

noCurlySuf f = nosuffix ".cy" f + nosuffix ".curly" f + nosuffix ".cyl" f
  where nosuffix s s' = if t==s then Just h else Nothing
          where (h,t) = splitAt (length s'-length s) s'

