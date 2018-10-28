{-# LANGUAGE CPP, NoMonomorphismRestriction, OverloadedStrings, ScopedTypeVariables, DeriveGeneric, ConstraintKinds, UndecidableInstances, PatternSynonyms #-}
module CaPriCon.Run where

import Definitive
import Language.Format
import Algebra.Monad.Concatenative
import Data.CaPriCon
import GHC.Generics (Generic)

class Monad m => MonadSubIO io m where
  liftSubIO :: io a -> m a
instance MonadSubIO IO IO where liftSubIO = id
instance MonadSubIO io m => MonadSubIO io (ConcatT st b o s m) where
  liftSubIO ma = lift $ liftSubIO ma

type MaxDelta = Int
type UniverseConstraint = [Maybe MaxDelta]
data UniverseConstraints = UniverseConstraints [UniverseConstraint]
instance Semigroup UniverseConstraints where
  UniverseConstraints x + UniverseConstraints y = UniverseConstraints $ zipWith (zipWith (\_x _y -> zipWith max _x _y + _x + _y)) x y
instance Monoid UniverseConstraints where zero = UniverseConstraints (repeat (repeat Nothing))
data COCValue io str = COCExpr (ContextNode str)
                     | COCNull | COCError str
                     | COCConvertible (Maybe (Int,Int))
                     | COCDir (NodeDir str ([str],StackVal str (COCBuiltin io str) (COCValue io str)))
                     deriving Generic
instance (ListSerializable s,ListSerializable b,ListSerializable a) => ListSerializable (StackVal s b a)
instance (IsCapriconString s,ListFormat s,ListFormat b,ListFormat a) => ListFormat (StackVal s b a)
instance (ListSerializable b) => ListSerializable (StackBuiltin b)
instance (ListFormat b) => ListFormat (StackBuiltin b)
instance (ListSerializable a) => ListSerializable (Opaque a)
instance (ListFormat a) => ListFormat (Opaque a)

instance ListSerializable str => ListSerializable (COCValue io str)
instance (IsCapriconString str,ListFormat str,IOListFormat io str) => ListFormat (COCValue io str)
type COCDict io str = Map str (StackVal str (COCBuiltin io str) (COCValue io str))

pattern StackCOC v = StackExtra (Opaque v)

takeLast n l = drop (length l-n) l

showStackVal :: IsCapriconString str => (NodeDoc str -> str) -> NodeDir str ([str],StringPattern str) -> [(str,Node str)] -> StackVal str (COCBuiltin io str) (COCValue io str) -> str
showStackVal toRaw dir ctx = fix $ \go _x -> case _x of
  StackCOC _x -> case _x of
    COCExpr (ContextNode d e) -> -- "<"+show d+">:"+
      toRaw $ showNode' dir (map (second snd) $ takeLast d (freshContext ctx)) e
    COCNull -> "(null)"
    COCError e -> "<!"+e+"!>"
    COCDir d -> fromString $ show d
    COCConvertible conv -> fromString $ show conv
  StackSymbol s -> fromString $ show s
  StackInt n -> fromString $ show n
  StackList l -> "["+intercalate "," (map go l)+"]"
  StackDict d -> "[<"+intercalate "," (map (\(k,v) -> k+": "+go v) (d^.ascList))+">]"
  StackProg p -> "{ "+intercalate " " p+" }"
  _ -> fromString $ show _x
data COCBuiltin io str = COCB_Print
                       | COCB_Open (ReadImpl io str str) | COCB_ExecModule (WriteImpl io str str)
                       | COCB_Cache (ReadImpl io str [Word8]) (WriteImpl io str [Word8])
                       | COCB_ToInt | COCB_Concat | COCB_Uni | COCB_Hyp
                       | COCB_Quit | COCB_Var
                       | COCB_Ap | COCB_Bind Bool BindType
                       | COCB_TypeOf | COCB_Mu | COCB_Convertible
                       | COCB_HypBefore | COCB_Subst | COCB_Rename
                       | COCB_ContextVars
                       | COCB_GetShowDir | COCB_SetShowDir | COCB_InsertNodeDir
                       | COCB_Format
                       deriving (Show,Generic)
data ReadImpl io str bytes = ReadImpl (str -> io (Maybe bytes))
data WriteImpl io str bytes = WriteImpl (str -> bytes -> io ())
instance Show (ReadImpl io str bytes) where show _ = "#<open>"
instance Show (WriteImpl io str bytes) where show _ = "#<write>"

type ListSerializable a = (Serializable [Word8] a)
type ListFormat a = (Format [Word8] a)
type IOListFormat io str = (ListFormat (ReadImpl io str str), ListFormat (WriteImpl io str str),
                            ListFormat (ReadImpl io str [Word8]), ListFormat (WriteImpl io str [Word8]))
instance Serializable [Word8] (ReadImpl io str bytes) where encode _ _ = zero
instance Serializable [Word8] (WriteImpl io str bytes) where encode _ _ = zero
instance ListSerializable str => ListSerializable (COCBuiltin io str)
instance (ListFormat str,IOListFormat io str) => ListFormat (COCBuiltin io str)

htmlQuote :: IsCapriconString str => str -> str
htmlQuote = fromString . foldMap qChar . toString
  where qChar '<' = "&lt;"
        qChar '>' = "&gt;"
        qChar '&' = "&amp;"
        qChar c = [c]
stringWords :: IsCapriconString str => str -> [str]
stringWords = map fromString . fromBlank . toString
  where fromBlank (c:t) | c `elem` [' ', '\t', '\r', '\n'] = fromBlank t
                        | c == '"' = fromQuote id t
                        | otherwise = fromWChar (c:) t
        fromBlank "" = []
        fromQuote k ('"':t) = ('"':k "\""):fromBlank t
        fromQuote k ('\\':c:t) = fromQuote (k.(qChar c:)) t
          where qChar 'n' = '\n' ; qChar 't' = '\t' ; qChar x = x
        fromQuote k (c:t) = fromQuote (k.(c:)) t
        fromQuote k "" = ['"':k "\""]
        fromWChar k (c:t) | c `elem` [' ', '\t', '\r', '\n'] = k "":fromBlank t
                          | otherwise = fromWChar (k.(c:)) t
        fromWChar k "" = [k ""]

literate :: forall str. IsCapriconString str => Parser String [str]
literate = intercalate [":s\n"] <$> sepBy' (cmdline "> " <+? cmdline "$> " <+? commentline) (single '\n')
  where
    wrapResult :: Bool -> [str] -> [str]
    wrapResult isParagraph l = (if isParagraph then ":rbp" else ":rbs") : l + [if isParagraph then ":rep" else ":res"]
    cmdline :: Parser String () -> Parser String [str]
    cmdline pre = map (\x -> [":cp"+intercalate "\n" (map fst x)]
                             + wrapResult True (foldMap snd x))
                  (sepBy1' go (single '\n'))
      where go = do pre; many' (noneOf ['\n']) <&> \x -> (fromString x,map fromString (stringWords x+["steps."]))
    commentline = map (foldMap (pure . (":s"+) <|> \(x,t) -> t+[":cs"+x])) $ (<* lookingAt eol)
      $ many' (map (Left . fromString) (many1' (noneOf ['{','\n'] <+?
                                                (fill '{' $ single '{' <* lookingAt (noneOf ['{']))))
                <+? map Right (between "{{" "}}"
                                (many1' (noneOf ['}'] <+? fill '}' (single '}' <* lookingAt (noneOf ['}'])))
                                 <&> \x -> (fromString x,wrapResult False (stringWords (fromString x)+["mustache."])))))

data COCState str = COCState {
  _endState :: Bool,
  _context :: [(str,Node str)],
  _showDir :: NodeDir str ([str],StringPattern str),
  _outputText :: str -> str
  }
endState :: Lens' (COCState str) Bool
endState = lens _endState (\x y -> x { _endState = y })
context :: Lens' (COCState str) [(str,Node str)]
context = lens _context (\x y -> x { _context = y })
showDir :: Lens' (COCState str) (NodeDir str ([str],StringPattern str))
showDir = lens _showDir (\x y -> x { _showDir = y })
outputText :: Lens' (COCState str) (str -> str)
outputText = lens _outputText (\x y -> x { _outputText = y })

pushError :: MonadStack (COCState str) str (COCBuiltin io str) (COCValue io str) m => str -> m ()
pushError s = runStackState $ modify $ (StackCOC (COCError s):)

runInContext :: Env str -> MaybeT ((->) (Env str)) a -> Maybe a
runInContext c v = (v^..maybeT) c

modifyAllExprs :: MonadStack (COCState str) str (COCBuiltin io str) (COCValue io str) m
               => (ContextNode str -> ContextNode str) -> m ()
modifyAllExprs f = do
  let modStack (StackCOC (COCExpr e)) = StackCOC (COCExpr (f e))
      modStack (StackDict d) = StackDict (map modStack d)
      modStack (StackList l) = StackList (map modStack l)
      modStack x = x
  runStackState $ modify $ map modStack
  runDictState $ modify $ map modStack
modifyCOCEnv :: MonadStack (COCState str) str (COCBuiltin io str) (COCValue io str) m
          => Maybe (ContextNode str -> ContextNode str,Env str) -> m ()
modifyCOCEnv Nothing = unit
modifyCOCEnv (Just (modE,ctx)) = do
  runExtraState (context =- ctx)
  modifyAllExprs modE

runCOCBuiltin :: (MonadSubIO io m,IsCapriconString str, MonadStack (COCState str) str (COCBuiltin io str) (COCValue io str) m,IOListFormat io str,ListFormat str) => COCBuiltin io str -> m ()
runCOCBuiltin COCB_Quit = runExtraState (endState =- True)
runCOCBuiltin COCB_Print = do
  s <- runStackState get
  for_ (take 1 s) $ \case
    StackSymbol s' -> runExtraState (outputText =~ \o t -> o (s'+t))
    _ -> return ()

runCOCBuiltin COCB_Format = do
  ex <- runExtraState get
  let format ('%':'s':s) (StackSymbol h:t) = first (h+) (format s t)
      format ('%':'v':s) (x:t) = first (showStackVal doc2raw (ex^.showDir) (ex^.context) x+) (format s t)
      format ('%':'l':s) (x:t) = first (showStackVal doc2latex (ex^.showDir) (ex^.context) x+) (format s t)
      format (c:s) t = first (fromString [c]+) (format s t)
      format "" t = ("",t)
  runStackState $ modify $ \case
    StackSymbol s:t -> uncurry ((:) . StackSymbol) (format (toString s) t)
    st -> st

runCOCBuiltin (COCB_Open (ReadImpl getResource)) = do
  s <- runStackState get
  case s of
    StackSymbol f:t -> do
      xs <- liftSubIO (getResource (f+".md")) >>= maybe undefined return . matches Just literate . maybe "" toString
      runStackState (put (StackProg xs:t))
    _ -> return ()
                     
runCOCBuiltin COCB_ToInt = runStackState $ modify $ \case
  StackSymbol s:t -> StackInt (read (toString s)):t
  st -> st
runCOCBuiltin COCB_Concat = runStackState $ modify $ \case
  StackSymbol s:StackSymbol s':t -> StackSymbol (s'+s):t
  st -> st

runCOCBuiltin COCB_Uni = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackInt u:t | Just x <- runInContext ctx (mkUniverse u) -> StackCOC (COCExpr x):t
    st -> st
runCOCBuiltin COCB_Var = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackSymbol name:t | Just e <- runInContext ctx (pullTerm =<< mkVariable name) -> StackCOC (COCExpr e):t
    st -> st
runCOCBuiltin COCB_Ap = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    (StackCOC (COCExpr f):StackCOC (COCExpr x):t)
      | Just e <- runInContext ctx (mkApply f x) -> StackCOC (COCExpr e):t
    x -> x
runCOCBuiltin (COCB_Bind close bt) = do
  ctx <- runExtraState (getl context) 
  let dctx = length ctx
      setVal (StackCOC (COCExpr e@(ContextNode d _)))
        | d==dctx || not close
        , Just e' <- runInContext ctx (mkBind bt e) = StackCOC (COCExpr e')
      setVal (StackDict dict) = StackDict (map setVal dict)
      setVal (StackList l) = StackList (map setVal l)
      setVal x = x
      setStack (x:t) = setVal x:if close then setStack t else t
      setStack [] = []
                                                         
  ctx' <- runStackState $ id <~ map (,if close && nonempty ctx then tail ctx else ctx) setStack
  runDictState $ modify $ map setVal
  runExtraState (context =- ctx')
runCOCBuiltin COCB_Mu = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackCOC (COCExpr e):t | Just e' <- runInContext ctx (mkMu e) -> StackCOC (COCExpr e'):t
                           | otherwise -> StackCOC COCNull:t
    st -> st
runCOCBuiltin COCB_TypeOf = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackCOC (COCExpr e):t | Just e' <- runInContext ctx (checkType e) -> StackCOC (COCExpr e'):t
                           | otherwise -> StackCOC COCNull:t
    st -> st
runCOCBuiltin COCB_Convertible = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackCOC (COCExpr e):StackCOC (COCExpr e'):t ->
      StackCOC (COCConvertible (runInContext ctx $ conversionDelta e e')):t
    st -> st

runCOCBuiltin (COCB_ExecModule (WriteImpl writeResource)) = do
  st <- runStackState get
  case st of
    StackSymbol f:StackProg p:t -> do
      old <- runDictState get
      oldH <- runExtraState (outputText <~ \x -> (id,x))
      traverse_ (execSymbol runCOCBuiltin outputComment) p
      new <- runDictState (id <~ (old,))
      newH <- runExtraState (outputText <~ \x -> (oldH,x))
      liftSubIO $ writeResource f (newH "")
      runStackState $ put $ StackDict new:t
    _ -> return ()

runCOCBuiltin (COCB_Cache (ReadImpl getResource) (WriteImpl writeResource)) = do
  st <- runStackState get
  case st of
    StackSymbol f:StackProg p:t -> do
      runStackState (put t)
      liftSubIO (getResource (f+".blob")) >>= \case
        Just res | Just v <- matches Just datum res -> runStackState $ modify $ (v:)
        _ -> do
          traverse_ (execSymbol runCOCBuiltin outputComment) p
          st' <- runStackState get
          case st' of
            v:_ -> liftSubIO $ writeResource (f+".blob") (serialize v)
            _ -> unit
    _ -> pushError "Invalid argument types for builtin 'cache'. Usage: <prog> <string> cache."

runCOCBuiltin COCB_Hyp = do
  ctx <- runExtraState (getl context)
  s <- runStackState $ id <~ \case
    StackSymbol name:StackCOC (COCExpr e):t
      -> (t,runInContext ctx (insertHypBefore Nothing name e))
    st -> (st,Nothing)
  modifyCOCEnv s
  
runCOCBuiltin COCB_HypBefore = do
  ctx <- runExtraState (getl context)
  s <- runStackState $ id <~ \case
    StackSymbol h:StackSymbol h':StackCOC (COCExpr e):t
      -> (t,runInContext ctx (insertHypBefore (Just h) h' e))
    st -> (st,Nothing)
  modifyCOCEnv s
runCOCBuiltin COCB_Subst = do
  ctx <- runExtraState (getl context)
  s <- runStackState $ id <~ \case
    StackSymbol h:StackCOC (COCExpr e):t -> (t,runInContext ctx (substHyp h e))
    st -> (st,Nothing)
  modifyCOCEnv s

runCOCBuiltin COCB_Rename = do
  ctx <- runExtraState (getl context)
  ctx' <- runStackState $ id <~ \case
    StackSymbol s:StackSymbol s':t -> (t,map (\(n',(n,v)) -> (if n'==s then s' else n, v)) (freshContext ctx))
    st -> (st,ctx)
  runExtraState (context =- ctx')
runCOCBuiltin COCB_ContextVars = do
  ctx <- runExtraState (getl context)
  runStackState $ modify (StackList (map (StackSymbol . fst) (freshContext ctx)):)

runCOCBuiltin COCB_GetShowDir = do
  dir <- runExtraState (getl showDir)
  runStackState $ modify $ (StackCOC (COCDir (map (\(c,l) -> (c,StackSymbol (intercalate " " $ map (id <|> head . flip drop c) l))) dir)):)
runCOCBuiltin COCB_SetShowDir = do
  mod' <- runStackState $ id <~ \case
    StackCOC (COCDir d):t -> (t,showDir =- map (\(c,StackSymbol ws) -> (c,[case select ((==w) . fst) (zip c [0..]) of
                                                                                        (_,i):_ -> Right i
                                                                                        _ -> Left w
                                                                                     | w <- map fromString $ words (toString ws)])) d)
    st -> (st,return ())
  runExtraState mod'
runCOCBuiltin COCB_InsertNodeDir = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    x:StackCOC (COCExpr (ContextNode d e)):StackCOC (COCDir dir):t ->
      StackCOC (COCDir (insert e (map fst (takeLast d ctx),x) dir)):t
    st -> st

cocDict :: forall io str. IsCapriconString str => str -> (str -> io (Maybe str)) -> (str -> io (Maybe [Word8])) -> (str -> str -> io ()) -> (str -> [Word8] -> io ()) -> COCDict io str
cocDict version getResource getBResource writeResource writeBResource =
  mkDict ((".",StackProg []):("steps.",StackProg []):("mustache.",StackProg []):("version",StackSymbol version):
           [(x,StackBuiltin b) | (x,b) <- [
               ("def"                     , Builtin_Def                           ),
               ("$"                       , Builtin_DeRef                         ),
               ("lookup"                  , Builtin_Lookup                        ),
               ("exec"                    , Builtin_Exec                          ),
               ("quote"                   , Builtin_Quote                         ),

               ("stack"                   , Builtin_Stack                         ),
               ("clear"                   , Builtin_Clear                         ),
               ("shift"                   , Builtin_Shift                         ),
               ("shaft"                   , Builtin_Shaft                         ),
               ("pop"                     , Builtin_Pop                           ),
               ("popn"                    , Builtin_PopN                          ),
               ("dup"                     , Builtin_Dup                           ),
               ("dupn"                    , Builtin_DupN                          ),
               ("swap"                    , Builtin_Swap                          ),
               ("swapn"                   , Builtin_SwapN                         ),
               ("pick"                    , Builtin_Pick                          ),

               ("["                       , Builtin_ListBegin                     ),
               ("]"                       , Builtin_ListEnd                       ),
               
               ("io/exit"                 , Builtin_Extra COCB_Quit               ),
               ("io/print"                , Builtin_Extra COCB_Print              ),
               ("io/source"               , Builtin_Extra (COCB_Open (ReadImpl getResource))), 
               ("io/cache"                , Builtin_Extra (COCB_Cache (ReadImpl getBResource) (WriteImpl writeBResource))),
                 
               ("string/format"           , Builtin_Extra COCB_Format             ),
               ("string/to-int"           , Builtin_Extra COCB_ToInt              ),
               
               ("arith/+"                 , Builtin_Add                           ),
               ("arith/-"                 , Builtin_Sub                           ),
               ("arith/*"                 , Builtin_Mul                           ),
               ("arith/div"               , Builtin_Div                           ),
               ("arith/mod"               , Builtin_Mod                           ),
               ("arith/sign"              , Builtin_Sign                          ),
               
               ("list/each"               , Builtin_Each                          ),
               ("list/range"              , Builtin_Range                         ),

               ("dict/vocabulary"         , Builtin_CurrentDict                   ),
               ("dict/empty"              , Builtin_Empty                         ),
               ("dict/insert"             , Builtin_Insert                        ),
               ("dict/delete"             , Builtin_Delete                        ),
               ("dict/keys"               , Builtin_Keys                          ),
               ("dict/module"             , Builtin_Extra (COCB_ExecModule (WriteImpl writeResource))),

               ("term-index/pattern-index"     , Builtin_Extra COCB_GetShowDir         ),
               ("term-index/set-pattern-index" , Builtin_Extra COCB_SetShowDir         ),
               ("term-index/index-insert"      , Builtin_Extra COCB_InsertNodeDir      ),
               
               ("term/universe"            , Builtin_Extra COCB_Uni                ),
               ("term/variable"            , Builtin_Extra COCB_Var                ),
               ("term/apply"               , Builtin_Extra COCB_Ap                 ),
               ("term/lambda"              , Builtin_Extra (COCB_Bind False Lambda )),
               ("term/forall"              , Builtin_Extra (COCB_Bind False Prod   )  ),
               ("term/mu"                  , Builtin_Extra COCB_Mu                 ),
               ("term/convertible"         , Builtin_Extra COCB_Convertible        ),

               ("context/intro"           , Builtin_Extra COCB_Hyp                ),
               ("context/intro-before"    , Builtin_Extra COCB_HypBefore          ),
               ("context/extro-lambda"    , Builtin_Extra (COCB_Bind True Lambda  ) ),
               ("context/extro-forall"    , Builtin_Extra (COCB_Bind True Prod    )   ),
               ("context/rename"          , Builtin_Extra COCB_Rename             ),
               ("context/substitute"      , Builtin_Extra COCB_Subst              ),
               ("context/type"            , Builtin_Extra COCB_TypeOf             ),
               ("context/hypotheses"      , Builtin_Extra COCB_ContextVars        )
               ]])
  where mkDict :: [(str,StackVal str (COCBuiltin io str) (COCValue io str))] -> Map str (StackVal str (COCBuiltin io str) (COCValue io str))
        mkDict = foldr addElt (c'map zero)
        addElt (x,v) = atP (first fromString $ splitPath $ toString x) %- Just v
        splitPath ('/':x) = ("",uncurry (:) (first fromString $ splitPath x))
        splitPath (h:t) = let ~(w,l) = splitPath t in (h:w,l)
        splitPath [] = ("",[])
        atP (h,[]) = at h
        atP (h,x:t) = at h.l'Just (StackDict zero).t'StackDict.atP (x,t)

outputComment c = (runExtraState $ do outputText =~ (\o t -> o (commentText+t)))
  where commentText = case toString c of
          'r':'b':p:[] -> let x = if p=='p' then "paragraph" else ""
                              tag = if p=='p' then "div" else "span"
                          in "<"+tag+" class=\"capricon-"+x+"result\">"
          'r':'e':p:[] -> "</"+(if p=='p' then "div" else "span")+">"
          'c':'p':_ -> let nlines = length (lines (toString c))
                       in wrapStart True nlines+"<div class=\"capricon-steps\"><pre class=\"capricon capricon-paragraph capricon-context\">"
                          +htmlQuote (drop 2 c)+"</pre>"+userInput+"</div>"+wrapEnd
          'c':'s':_ -> wrapStart False 1+"<code class=\"capricon\">"+htmlQuote (drop 2 c)+"</code>"+wrapEnd
          's':_ -> drop 1 c
          _ -> ""

        wrapStart isP nlines =
          let hide = if isP then "hideparagraph" else "hidestache"
          in "<label class=\"hide-label\"><input type=\"checkbox\" class=\"capricon-hide\" checked=\"checked\"/><span class=\"capricon-"
             + hide +"\"></span><span class=\"capricon-reveal\" data-linecount=\""
             + fromString (show nlines)+"\">"
        wrapEnd = "</span></label>"
        userInput = "<div class=\"user-input\"><button class=\"capricon-trigger\">Open/Close console</button><span class=\"capricon-input-prefix\">Evaluate in this context (press Enter to run):</span><input type=\"text\" class=\"capricon-input\" /><pre class=\"capricon-output\"></pre></div>"
  
