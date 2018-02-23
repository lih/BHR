{-# LANGUAGE TypeFamilies, StandaloneDeriving #-}
module Curly.Core.Library(
  -- * Modules
  -- ** Nodes 
  ModDir(..),i'ModDir,Module,Mountain,Context,context,localContext,
  atM,atMs,fromPList,
  -- ** Leaves
  ModLeaf,SourcePos,SourceRange(..),
  undefLeaf,undefSymLeaf,leafVal,leafDoc,leafPos,leafType,leafStrictness,leafIsFamily,
  -- * Libraries
  GlobalID(..),isLibData,
  Metadata(..),Library,metadata,imports,exports,symbols,implicits,
  addImport,addExport,setExports,defSymbol,libSymbol,
  exprIn,optExprIn,builtinLibs,
  -- ** Documentation
  LeafExpr,DocNode(..),Documentation,docNodeAttrs,docNodeSubs,descSymbol,docAtom,docLine,mkDoc,
  -- * Files
  FileLibrary,flLibrary,flID,flBytes,flFromSource,flSource,
  rawLibrary,fileLibrary,  
  -- * Repositories
  Template,defaultTemplate,showTemplate,showDummyTemplate,
  findLib,findSym,availableLibs,libraryVCS
  ) where

import Curly.Core.Security.SHA256
import Curly.Core
import Curly.Core.Documentation
import Curly.Core.VCS
import Curly.Core.Security
import Curly.Core.Annotated
import Data.IORef
import Language.Format
import Control.Concurrent (forkIO)
import GHC.Conc (par)
import Control.Concurrent.MVar

curlyLibVersion :: Int
curlyLibVersion = 11

newtype Chunked a = Chunked { getChunked :: a }
instance Serializable a => Serializable (Chunked a) where
  encode (Chunked a) = encode (serialize a)
instance Format a => Format (Chunked a) where
  datum = datum <&> \x -> maybe (error "Invalid chunked value") Chunked (matches Just (datum <* (guard . (==zero) =<< remaining)) x)
newtype Extension a = Extension (Chunked a)
                    deriving Serializable
instance Format a => Format (Extension a) where
  datum = coerceDatum Extension


newtype ModDir s a = ModDir [(s,a)]
                      deriving (Semigroup,Monoid,Show)
i'ModDir :: Iso [(s,a)] [(s',a')] (ModDir s a) (ModDir s' a')
i'ModDir = iso (\(ModDir m) -> m) ModDir 
type Module a = Free (ModDir String) a
instance Documented a => Documented (Module a) where
  document (Join (ModDir l)) = docTag' "ul" (map (docTag "li" [("class","modVal")] . pure . doc') l)
    where doc' (s,Pure n) | s==pretty n = Pure s
                          | otherwise = document n
          doc' (s,Join (ModDir l')) = docTag' "p"
                                      [docTag "ln" [("class","modName")] [Pure (s+":")]
                                      ,docTag' "ul"  (map (docTag "li" [("class","modVal")] . pure . doc') l')]
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
atM s a k (ModDir d) = map ModDir $ for d' $ \(s',a') -> (s',) <$> if s'==s then k a' else return a'
  where d' | has (at s.traverse) d = d
           | otherwise = (s,a):d
atMs :: Eq s => [s] -> Traversal' (Free (ModDir s) a) (Free (ModDir s) a)
atMs [] k x = k x
atMs (s:ss) k (Join m) = Join<$>(atM s (Join zero).atMs ss) k m
atMs (s:ss) k _ = Join<$>(atM s (Join zero).atMs ss) k zero

fromPList :: Eq s => [([s], Free (ModDir s) a)] -> Free (ModDir s) a
fromPList l = compose [atMs p %- v | (p,v) <- l] (Join zero)

libraryCache :: IORef (Map LibraryID FileLibrary)
libraryCache = newIORef zero^.thunk

registerLib :: FileLibrary -> FileLibrary
registerLib l = by thunk $ do
  let i = l^.flID
  logLine Debug $ format "Registering library %s" (show i)
  i`seq`modifyIORef libraryCache (insert i l)
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
  _leafIsFamily :: Bool,
  _leafStrictness :: Strictness s,
  _leafExtension :: Extension (),
  _leafVal :: a
  }
                 deriving Generic
instance Functor (ModLeaf s) where
  map = warp leafVal 
instance Foldable (ModLeaf s) where fold l = l^.leafVal
instance Traversable (ModLeaf s) where sequence l = leafVal id l
instance (Identifier s,Serializable s,Serializable a) => Serializable (ModLeaf s a) where
  encode (ModLeaf a b c d e f g) = encode (Chunked a)+encode b+encode (Chunked c)+encode d+encode e+encode f+encode (Chunked g)
instance (Identifier s,Format s,Format a) => Format (ModLeaf s a) where
  datum = (\(Chunked a) b (Chunked c) d e f (Chunked g) -> ModLeaf a b c d e f g)
          <$>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum
instance (Identifier s,Identifier s') => HasIdents s s' (ModLeaf s a) (ModLeaf s' a) where
  ff'idents = leafAllSymbols.(l'1.ff'idents .+ l'2.ff'idents)

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
leafIsFamily :: Lens' (ModLeaf s a) Bool
leafIsFamily = lens _leafIsFamily (\x y -> x { _leafIsFamily = y })
leafVal :: Lens a b (ModLeaf s a) (ModLeaf s b)
leafVal = lens _leafVal (\x y -> x { _leafVal = y })
leafStrictness :: Lens' (ModLeaf s a) (Strictness s)
leafStrictness = leafAllSymbols.l'2
leafType :: Lens' (ModLeaf s a) (Type s)
leafType = leafAllSymbols.l'1
leafAllSymbols :: Lens (Type s,Strictness s) (Type s',Strictness s') (ModLeaf s a) (ModLeaf s' a)
leafAllSymbols = lens (liftA2 (,) _leafType _leafStrictness) (\x (y,z) -> x { _leafType = y , _leafStrictness = z })

type LeafExpr s = ModLeaf s (NameExpr s)

type Context = Module (GlobalID,LeafExpr GlobalID)

data Library = Library {
  _metadata :: Metadata,
  _imports :: Context,
  _symbols :: Map String (LeafExpr GlobalID),
  _externalSyms :: Map String GlobalID,
  _implicits :: InstanceMap GlobalID (Maybe LibraryID,LeafExpr GlobalID),
  _exports :: Context,
  _libExtension :: Extension ()
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
  Library syn i s es is e ext + Library _ i' s' es' is' e' _ = Library syn (i+i') (s+s') (es+es') (is+is') (e+e') ext
instance Monoid Library where
  zero = Library (Metadata zero) zero zero zero zero zero (Extension (Chunked ()))
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
             + let (m,(a,b,c,d,e,f,g,h)) = l^.scoped.withStrMap
                   syn = fromMaybe "" (a^?at "synopsis".t'Just.t'Pure)
               in foldMap encode (syn+"\n") + encode (curlyLibVersion,Compressed (m,
                                                                                  Chunked (delete "synopsis" a),
                                                                                  Chunked (map Chunked b),
                                                                                  Chunked c,
                                                                                  d,
                                                                                  Chunked e,
                                                                                  f,g,h))
instance Format Library where
  datum = do
    traverse_ (\c -> datum >>= guard . (c==)) cylMagic
    syn <- many' (datum <*= guard . (/='\n')) <* (datum >>= guard . (=='\n'))
    datum >>= \(vers,Compressed (m,Chunked a,Chunked b,Chunked c,d,Chunked e,f,g,h)) -> do
      guard (vers >= 11 && vers <= curlyLibVersion)
      return $ (m,(insert "synopsis" (Pure syn) a,map getChunked b,c,d,e,f,g,h))^..scoped.withStrMap

type ExprRep s = ModLeaf s (Expression s (s,Maybe (Symbol s)))
type LibRep s = (Metadata,Module s
                ,Map String (ExprRep s)
                ,Map String s
                ,InstanceMap s (ExprRep s)
                ,Set LibraryID
                ,Module s,Extension ())
scoped :: Iso' Library (LibRep GlobalID)
scoped = iso f g
  where f (Library syn i s es is e ext) = (syn,map fst i,map2 toExpr s,es,map2 toExpr (filterInsts is),instDeps,map fst e,ext)
          where toSym (sid@(GlobalID _ (Just _)),Pure (Builtin _ (B_Bytes _))) = (sid,Nothing)
                toSym (sid,Pure sym) = (sid,Just sym)
                toSym (sid,_) = (sid,Nothing)
                  
                toExpr = map toSym . c'Expression . semantic
                filterInsts = map snd . warp ascList (\l -> [x | x@(_,(Nothing,_)) <- l])
                instDeps = c'set $ fromKList [k | (Just k,_) <- toList is]
        g (syn,i',s',es,is',isd,e',ext) = Library syn i s es is e ext
          where symVal (GlobalID _ (Just (sname,l))) = fromMaybe (error $ "Couldn't find library "+show l) (findLib l)
                                                       ^.flLibrary.symbols.at sname.l'Just (undefSymLeaf sname (Just l))
                symVal (GlobalID sname Nothing) = s^.at sname.l'Just (undefLeaf (format "Undefined local symbol %s" sname))
                fromSym (sid,Just sym) = (sid,Pure sym)
                fromSym (sid,Nothing) = (sid,Join (symVal sid^.leafVal))
                fromExpr = withType . map (_rawNameExpr . semantic . c'Expression . map fromSym)
                withType lf = lf & set (leafVal.t'exprType) (lf^.leafType)
                i = map (\sid -> (sid,symVal sid)) i'
                e = map (\sid -> (sid,symVal sid)) e'
                s = map fromExpr s'
                is = map ((Nothing,) . fromExpr) is' + fold [fl^.flLibrary.implicits
                                                            | Just fl <- map findLib (keys isd)]

withStrMap :: Iso' (LibRep GlobalID) (Map Int GlobalID,LibRep Int)
withStrMap = iso f g
  where f (n,i,v,ev,iv,ivd,o,ext) = let ((_,strs),(i',v',ev',iv',o')) = yb state foo zero
                                    in (toMap (commute strs),(n,i',v',ev',iv',ivd,o',ext))
          where strId s = id <~ \(sz,m) -> case lookup s m of
                  Just sid -> ((sz,m),sid)
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
        g (m,(n,i',v',ev',iv',ivd,o',ext)) = (n,map idSym i',v,ev,iv,ivd,map idSym o',ext)
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
withPrevIdents :: String -> Module a -> Module (String,a)
withPrevIdents p (Pure a) = Pure (p,a)
withPrevIdents _ (Join (ModDir d)) = Join (ModDir [(s,withPrevIdents s x) | (s,x) <- d])
mapIdents :: (String -> GlobalID -> GlobalID) -> (GlobalID -> GlobalID) -> String -> Context -> Context
mapIdents sw f = mapC
  where mapDE = warp (leafType.ff'idents) f . warp leafVal mapE
        mapE = warp (from i'NameNode) (map (first f . warp (l'2.t'Pure.t'Builtin.l'2) mapB))
               . warp (t'exprType.ff'idents) f
        mapB (B_Foreign vals def) = B_Foreign (map f vals) (f def)
        mapB x = x
        mapC _ (Join (ModDir m)) = Join . ModDir $ warp each (\(s,e) -> (s,mapC s e)) m
        mapC s (Pure (i,e)) = Pure (sw s (f i),mapDE e)
context :: Mountain -> Context
context m = withPrevIdents "" m >>= \(n,fl) -> mapIdents (\s (GlobalID _ l) -> GlobalID s l) (setId (fl^.flID)) n (fl^.flLibrary.exports)
  where setId i (GlobalID n Nothing) = GlobalID n (Just (n,i))
        setId _ x = x
localContext :: (?mountain :: Mountain) => Context
localContext = context ?mountain
                    
undefSym :: NameExpr GlobalID
undefSym = mkSymbol (pureIdent "undefined",Pure (Builtin (builtinType B_Undefined) B_Undefined))
undefLeaf :: String -> LeafExpr GlobalID
undefLeaf msg = ModLeaf (nodoc msg) NoRange zero False noStrictness (Extension (Chunked ext)) undefSym
  where ext = ()
undefSymLeaf :: String -> Maybe LibraryID -> LeafExpr GlobalID
undefSymLeaf s ml = undefLeaf (format "Undocumented symbol %s%s" s (case ml of Just l -> format " in %s" (show l)
                                                                               Nothing -> ""))

addImport :: Context -> Library -> Library
addImport imp = warp imports (+imp) . warp symbols (fromAList (map2 snd newSyms)+)
                . warp externalSyms (fromAList (map2 fst newSyms)+)
  where f (i,e) = (identName i,(i,warp leafVal (\e' -> mkSymbol (i,Join e')) e))
        newSyms = map f (toList imp)
resolve :: Library -> Module String -> Context
resolve l e = map go e
  where go n = (fromMaybe (pureIdent n) (l^.externalSyms.at n),
                fromMaybe (undefSymLeaf n Nothing) (l^.symbols.at n))
addExport :: Module String -> Library -> Library
addExport e l = l & exports %~ (+resolve l e)
setExports :: Module String -> Library -> Library
setExports e l = l & exports %- resolve l e
defSymbol :: Semantic e String (String,Maybe (NameExpr GlobalID)) => String -> SourceRange -> Maybe (Type GlobalID) -> Bool -> e -> Library -> Library
defSymbol s r t isM e l = l & symbols.at s.l'Just (undefSymLeaf s Nothing) %~ set leafType tp . set leafVal e' . set leafPos r . set leafIsFamily isM
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
descSymbol s d l = l & symbols.at s.l'Just (undefSymLeaf s Nothing).leafDoc %- d

libSymbol :: Library -> GlobalID -> Maybe (LeafExpr GlobalID)
libSymbol l (GlobalID i Nothing) = l^.symbols.at i
libSymbol _ (GlobalID _ (Just (i,lid))) = findLib lid >>= \l -> l^.flLibrary.symbols.at i

builtinLibs :: [FileLibrary]
builtinLibs = map (\l -> rawLibrary False l (serialize l) Nothing)
              [blib_3]
  where
    blib_3 = blib_2 & set (sym ["string"] "showInt".leafDoc) (mkDoc "leafDoc" showIntDoc)
      where showIntDoc = "{section {title Show Number} {p Produces a string representation of its argument}}"
    blib_2 = blib_1 & setMeta ["author","email"] "marc.coiffier@curly-lang.net"
    blib_1 = blib_0 & setMeta ["version"] "0.5.2"
    sym p s = (symbols.at s.t'Just .+ exports.atMs (p + [s]).t'Pure.l'2)
    setMeta (h:t) v = metadata.from i'Metadata.at h.l'Just zero.at t %- Just (Pure v)
    setMeta [] _ = id
    blib_0 = zero
             & set symbols (fromAList [(foldl' (flip const) ph pt,v) | (ph:pt,(_,v)) <- allBuiltins])
             . set exports builtinsMod
             . set metadata (Metadata meta)
      where Join meta = fromAList [(["synopsis"],Pure "The Curly Builtin Library")
                                  ,(["author","name"],Pure "Marc Coiffier")
                                  ,(["author","email"],Pure "marc@coiffier.net")
                                  ,(["version"],Pure "0.5.1")]
            builtinsMod = fromPList (map2 Pure allBuiltins) 
            allBuiltins = [
              (["undefined"],(pureIdent "undefined",undefLeaf "The 'undefined' builtin")),
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
                where mkBLeaf n b d = (pureIdent n,undefLeaf "" & leafVal %- mkSymbol (pureIdent n,Pure (Builtin (builtinType b) b)) & leafDoc %- mkDoc "leafDoc" d)
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

type Template = Documentation
defaultTemplate :: Template
defaultTemplate = mkDoc "template"
                  $ unlines [
                    "{or \"{$ name}{or \" v{$ version}\" \"\"}: {$ synopsis}\"",
                    "    {ln {$ synopsis}}",
                    "    \"(data)\"}"
                    ]

showTemplate :: Terminal trm => trm -> Style -> DocPatterns -> Metadata -> Template -> Maybe String
showTemplate trm stl pats (Metadata d) tpl = map (docString trm stl) (evalDocWithPatterns pats (map2 Pure d) tpl)
showDummyTemplate :: Metadata -> Template -> Maybe String
showDummyTemplate = showTemplate DummyTerminal defaultStyle zero 

cacheName :: LibraryID -> String
cacheName l = cacheFileName curlyCacheDir (show l) "cyl"

libraryVCS :: IORef VCSBackend
libraryVCS = newIORef (fromMaybe (protoBackend "http" "curly-vc.coiffier.net/vcs")
                       (matches Just readable (envVar "" "CURLY_VCS")))^.thunk

forkValue :: IO a -> IO a
forkValue ma = do
  v <- newEmptyMVar
  _ <- forkIO $ ma >>= putMVar v
  return (takeMVar v^.thunk)

availableLibs :: IO [(LibraryID,Metadata)]
availableLibs = do
  conn <- readIORef libraryVCS
  ks <- getKeyStore
  allLibs <- for (ks^.ascList) $ \(kn,(_,k,_,_,_)) -> forkValue $ do
    StampedBranches _ branches <- getBranches conn k
    for (keys branches) $ \b -> forkValue $ do
      mcomm <- getBranch conn (Just (Left (k,b)))
      maybe (return zero) (getCommit conn) mcomm
        <&> map (at "repository".l'Just zero
                 %~ insert ["key-name"] (Pure kn)
                 . insert ["branch-name"] (Pure b))
  return $ fold (fold allLibs)^.ascList

readCachedLibrary :: LibraryID -> IO (Maybe FileLibrary)
readCachedLibrary l = do
  b <- trylog (return zero) $ readBytes (cacheName l)
  return $ do
    guard (isLibData l b)
    f <- matches Just datum b
    return (registerLib $ FileLibrary f b l False Nothing)

registerBuiltinsLib :: ()
registerBuiltinsLib = by thunk $ void (yb thunk (head builtinLibs))

findLib :: LibraryID -> Maybe FileLibrary
findLib l = registerBuiltinsLib`seq`by thunk $ do
  conn <- readIORef libraryVCS
  cache <- readIORef libraryCache
  return (lookup l cache)
    `orIO` readCachedLibrary l
    `orIO` do
      bs <- fromMaybe zero <$> vcbLoad conn (LibraryKey l)
      case guard (isLibData l bs) >> matches Just datum bs of
        Just f -> do
          createFileDirectory (cacheName l)
          writeBytes (cacheName l) bs
          return (Just (registerLib $ FileLibrary f bs l False Nothing))
        Nothing -> return Nothing
        
  where orIO ma mb = ma >>= maybe mb (return . Just)

findSym :: GlobalID -> Maybe (LeafExpr GlobalID)
findSym (GlobalID _ (Just (n,l))) = findLib l >>= by (flLibrary.symbols.at n)
findSym _ = Nothing
