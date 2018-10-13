{-# LANGUAGE CPP, NoMonomorphismRestriction #-}
module CaPriCon.Run where

import Definitive
import Language.Parser
import Algebra.Monad.Concatenative
import System.Environment (lookupEnv)
import Data.CaPriCon

takeLast n l = drop (length l-n) l

showStackVal dir ctx _x = case _x of
  StackExtra (Opaque _x) -> case _x of
    COCExpr d e -> -- "<"+show d+">:"+
      showNode' dir (map (second snd) $ takeLast d (freshContext ctx)) e
    COCNull -> "(null)"
    COCDir d -> show d
  StackSymbol s -> show s
  StackInt n -> show n
  _ -> show _x
data COCBuiltin = COCB_Print | COCB_Open | COCB_ExecModule | COCB_GetEnv
                | COCB_ToInt | COCB_Concat | COCB_Uni | COCB_Hyp
                | COCB_Quit | COCB_Var
                | COCB_Ap | COCB_Bind Bool BindType
                | COCB_TypeOf | COCB_Mu
                | COCB_HypBefore | COCB_Subst | COCB_Rename
                | COCB_ContextVars
                | COCB_GetShowDir | COCB_SetShowDir | COCB_InsertNodeDir
                | COCB_Format
                deriving Show

data COCState = COCState {
  _endState :: Bool,
  _context :: [(String,Node)],
  _showDir :: NodeDir ([String],StringPattern),
  _outputText :: String -> String
  }
endState :: Lens' COCState Bool
endState = lens _endState (\x y -> x { _endState = y })
context :: Lens' COCState [(String,Node)]
context = lens _context (\x y -> x { _context = y })
showDir :: Lens' COCState (NodeDir ([String],StringPattern))
showDir = lens _showDir (\x y -> x { _showDir = y })
outputText :: Lens' COCState (String -> String)
outputText = lens _outputText (\x y -> x { _outputText = y })

htmlQuote = foldMap qChar
  where qChar '<' = "&lt;"
        qChar '>' = "&gt;"
        qChar '&' = "&amp;"
        qChar c = [c]
stringWords = fromBlank
  where fromBlank (c:t) | c `elem` " \n\t\r" = fromBlank t
                        | c == '"' = fromQuote id t
                        | otherwise = fromWChar (c:) t
        fromBlank "" = []
        fromQuote k ('"':t) = ('"':k "\""):fromBlank t
        fromQuote k ('\\':c:t) = fromQuote (k.(qChar c:)) t
          where qChar 'n' = '\n' ; qChar 't' = '\t' ; qChar x = x
        fromQuote k (c:t) = fromQuote (k.(c:)) t
        fromQuote k "" = ['"':k "\""]
        fromWChar k (c:t) | c `elem` " \n\t\r" = k "":fromBlank t
                          | otherwise = fromWChar (k.(c:)) t
        fromWChar k "" = [k ""]

runCOCBuiltin COCB_Quit = runExtraState (endState =- True)
runCOCBuiltin COCB_Print = do
  s <- runStackState get
  for_ (take 1 s) $ \case
    StackSymbol s' -> runExtraState (outputText =~ \o t -> o (s'+t))
    _ -> return ()
runCOCBuiltin COCB_GetEnv = do
  st <- runStackState get
  case st of
    StackSymbol s:t -> do
      v <- lift $ lookupEnv s
      runStackState (put (StackSymbol (maybe "" id v):t))
    _ -> return ()

runCOCBuiltin COCB_Format = do
  ex <- runExtraState get
  let format ('%':'s':s) (StackSymbol h:t) = first (h+) (format s t)
      format ('%':'v':s) (x:t) = first (showStackVal (ex^.showDir) (ex^.context) x+) (format s t)
      format (c:s) t = first (c:) (format s t)
      format "" t = ("",t)
  runStackState $ modify $ \case
    StackSymbol s:t -> uncurry ((:) . StackSymbol) (format s t)
    st -> st

runCOCBuiltin COCB_Open = do
  s <- runStackState get
  case s of
    StackSymbol f:t -> do
      xs <- lift (try (return []) (try (readString f) (readString (f+".md")) >>= maybe undefined return . matches Just literate))
      runStackState (put (StackProg xs:t))
    _ -> return ()
  where literate = intercalate [":\n"] <$> sepBy' (cmdline (several "> ") <+? cmdline (several "$> ")
                                                            <+? commentline) (single '\n')
        wrapLabel hide x = "<label class=\"hide-label\"><input type=\"checkbox\" class=\"capricon-hide\" checked=\"checked\"/><span class=\"capricon-"+hide+"\"></span><span class=\"capricon-reveal\">"+x+"</span></label>"
        wrapResult tag x l = (":<"+tag+" class=\"capricon-"+x+"result\">") : l + [":</"+tag+">"]
        userInput = "<div class=\"user-input\"><input type=\"text\" class=\"capricon-input\" /><pre class=\"capricon-output\"></pre><button class=\"capricon-submit\">Run</button></div>"
        cmdline pre = map (\x -> (":"+wrapLabel "hideparagraph" ("<div class=\"capricon-steps\"><pre class=\"capricon capricon-paragraph capricon-context\">"+htmlQuote (intercalate "\n" (map fst x))+"</pre>"+userInput+"</div>"))
                                 : wrapResult "div" "paragraph" (foldMap snd x)) (sepBy1' go (single '\n'))
          where go = do pre; many' (noneOf "\n") <&> \x -> (x,stringWords x)
        commentline = map (foldMap (pure . (':':) <|> \(x,t) -> t+[':':(wrapLabel "hidestache" $ "<code class=\"capricon\">"+htmlQuote x+"</code>")])) $ (<* lookingAt eol)
          $ many' (map Left (many1' (noneOf "{\n" <+? (fill '{' $ single '{' <* lookingAt (noneOf "{"))))
                    <+? map Right (between (several "{{") (several "}}")
                                    (many1' (noneOf "}" <+? fill '}' (single '{' <* lookingAt (noneOf "}"))) <&> \x -> (x,wrapResult "span" "" (stringWords x)))))
                      
                      
runCOCBuiltin COCB_ToInt = runStackState $ modify $ \case
  StackSymbol s:t -> StackInt (read s):t
  st -> st
runCOCBuiltin COCB_Concat = runStackState $ modify $ \case
  StackSymbol s:StackSymbol s':t -> StackSymbol (s'+s):t
  st -> st

runCOCBuiltin COCB_Uni = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackInt n:t -> StackExtra (Opaque (COCExpr (length ctx) (Universe n))):t
    st -> st
runCOCBuiltin COCB_Var = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackSymbol name:t | Just i <- lookup name (zipWith (second . const) [0..] (freshContext ctx)) ->
                         StackExtra (Opaque (COCExpr (length ctx) (Cons (Ap (Sym i) [])))):t
    st -> st
runCOCBuiltin COCB_Ap = do
  ctx <- runExtraState (getl context)
  let adj d dd x = inc_depth (dd+nctx-d) x
      nctx = length ctx
      env = map snd ctx
  runStackState $ modify $ \case
    (StackExtra (Opaque (COCExpr df f)):StackExtra (Opaque (COCExpr dx x)):t) ->
      let x' = adj dx 1 x ; f' = adj df 0 f in
        StackExtra (Opaque (COCExpr nctx (subst f' (Cons (Ap (Sym 0) [x'])) env))):t
    x -> x
runCOCBuiltin (COCB_Bind close bt) = do
  ctx <- runExtraState (getl context) 
  let d = length ctx
      setVal (StackExtra (Opaque (COCExpr d' e')))
        | i <- d-d'
        , d==d' || not close
        , (_,(x,tx):_) <- splitAt i ctx
        = StackExtra (Opaque (COCExpr (d'-1) (Bind bt x tx e')))
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
  let locEnv d = map snd (takeLast d ctx)
  runStackState $ modify $ \case
    StackExtra (Opaque (COCExpr d e)):t -> 
      case type_of e (locEnv d) >>= \te -> mu_type te (locEnv d) of
        Just mte -> let args (Bind Prod _ tx e') = tx:args e'
                        args _ = []
                    in (:t) $ StackExtra $ Opaque $ COCExpr d $
                       subst e (Cons (Ap (Mu [] (args mte) (Ap (Sym 0) [])) [])) (locEnv d)
        Nothing -> StackExtra (Opaque COCNull):t
    st -> st
runCOCBuiltin COCB_TypeOf = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackExtra (Opaque (COCExpr d (Cons (Ap (Sym i) [])))):t
      | (_,ti):_ <- drop i ctx ->
          StackExtra (Opaque (COCExpr (d-i-1) ti)):t
    StackExtra (Opaque (COCExpr d e)):t -> (:t) $ StackExtra $ Opaque $ case type_of e (takeLast d (map snd ctx)) of
      Just te -> COCExpr d te
      Nothing -> COCNull
    st -> st

runCOCBuiltin COCB_ExecModule = do
  st <- runStackState get
  case st of
    StackSymbol f:StackProg p:t -> do
      old <- runDictState get
      oldH <- runExtraState (outputText <~ \x -> (id,x))
      traverse_ (execSymbol runCOCBuiltin outputComment) p
      new <- runDictState (id <~ (old,))
      newH <- runExtraState (outputText <~ \x -> (oldH,x))
      lift $ writeString f (newH "")
      runStackState $ put $ StackDict new:t
    _ -> return ()

runCOCBuiltin COCB_Hyp = do
  ass <- runStackState $ id <~ \case
    StackSymbol name:StackExtra (Opaque (COCExpr d typ)):t -> (t,Just (d,(name,typ)))
    st -> (st,Nothing)
  case ass of
    Just (d,x) -> runExtraState $ context =~ \ctx -> (second (inc_depth (length ctx-d)) x:ctx)
    Nothing -> return ()
runCOCBuiltin COCB_HypBefore = do
  ctx <- runExtraState (getl context)
  let csz = length ctx
      adj hi i j = if i+j>=hi then j+1 else j
  ctx' <- runStackState $ id <~ \case
    StackSymbol h:StackSymbol h':StackExtra (Opaque (COCExpr d e)):t
      | (hi,_):_ <- select ((==h) . fst . snd) (zip [0..] ctx)
      , all (>hi+d-csz) (free_vars e) ->
        let ctx' = foldr (\x k i -> case compare hi i of
                             LT -> x:k (i+1)
                             EQ -> second (adjust_depth (adj hi i)) x:(h',inc_depth (csz-(d+hi+1)) e):k (i+1)
                             GT -> second (adjust_depth (adj hi i)) x:k (i+1))
                   (\_ -> []) ctx 0
            adjE x@(StackExtra (Opaque (COCExpr d' e'))) =
              let i = csz-d'
              in if i<=hi then StackExtra (Opaque (COCExpr (d+1) (adjust_depth (adj (hi+1) i) e')))
                 else x
            adjE x = x
        in (map adjE t,ctx')
    st -> (st,ctx)
  runExtraState (context =- ctx')
runCOCBuiltin COCB_Subst = do
  ctx <- runExtraState (getl context)
  let csz = length ctx
  ctx' <- runStackState $ id <~ \case
    StackSymbol h:StackExtra (Opaque (COCExpr d e)):t
      | (hi,_):_ <- select ((==h) . fst . snd) (zip [0..] ctx)
      , all (>hi+d-csz) (free_vars e) ->
        let ctx' = foldr (\x k i env -> case compare i hi of
                             LT -> second (\xv -> substn e (hi-i) xv env) x:k (i+1) (tail env)
                             EQ -> k (i+1) (tail env)
                             GT -> x:k (i+1) (tail env)) (\_ _ -> []) ctx 0 (map snd ctx)
            adjE x@(StackExtra (Opaque (COCExpr d' e'))) =
              let i = csz - d'
              in if i<=hi then StackExtra (Opaque (COCExpr (d-1) ((substn e (hi-i) e' (map snd (drop i ctx))))))
                 else x
            adjE x = x
        in (map adjE t,ctx')
    st -> (st,ctx)
  runExtraState (context =- ctx')
runCOCBuiltin COCB_Rename = do
  ctx <- runExtraState (getl context)
  ctx' <- runStackState $ id <~ \case
    StackSymbol s:StackSymbol s':t -> (t,map (\(n,v) -> (if n==s then s' else n, v)) (ctx))
    st -> (st,ctx)
  runExtraState (context =- ctx')
runCOCBuiltin COCB_ContextVars = do
  ctx <- runExtraState (getl context)
  runStackState $ modify (StackList (map (StackSymbol . fst) (freshContext ctx)):)

runCOCBuiltin COCB_GetShowDir = do
  dir <- runExtraState (getl showDir)
  runStackState $ modify $ (StackExtra (Opaque (COCDir (map (\(c,l) -> (c,StackSymbol (intercalate " " $ map (id <|> head . flip drop c) l))) dir))):)
runCOCBuiltin COCB_SetShowDir = do
  mod' <- runStackState $ id <~ \case
    StackExtra (Opaque (COCDir d)):t -> (t,showDir =- map (\(c,StackSymbol ws) -> (c,[case select ((==w) . fst) (zip c [0..]) of
                                                                                        (_,i):_ -> Right i
                                                                                        _ -> Left w
                                                                                     | w <- words ws])) d)
    st -> (st,return ())
  runExtraState mod'
runCOCBuiltin COCB_InsertNodeDir = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    x:StackExtra (Opaque (COCExpr d e)):StackExtra (Opaque (COCDir dir)):t ->
      StackExtra (Opaque (COCDir (insert e (map fst (takeLast d ctx),x) dir))):t
    st -> st

data COCValue = COCExpr Int Node | COCNull | COCDir (NodeDir ([String],StackVal String COCBuiltin COCValue))

cocDict version =
  mkDict ((".",StackProg []):("version",StackSymbol version):
           [(x,StackBuiltin b) | (x,b) <- [
               ("def"                     , Builtin_Def                           ),
               ("$"                       , Builtin_DeRef                         ),
               ("lookup"                  , Builtin_Lookup                        ),
               ("exec"                    , Builtin_Exec                          ),
               ("quote"                   , Builtin_Quote                         ),

               ("stack"                   , Builtin_Stack                         ),
               ("clear"                   , Builtin_Clear                         ),
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
               ("io/open"                 , Builtin_Extra COCB_Open               ), 
               ("io/get-env"              , Builtin_Extra COCB_GetEnv             ),
                                     
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
               ("dict/module"             , Builtin_Extra COCB_ExecModule         ),

               ("term-index/pattern-index"     , Builtin_Extra COCB_GetShowDir         ),
               ("term-index/set-pattern-index" , Builtin_Extra COCB_SetShowDir         ),
               ("term-index/index-insert"      , Builtin_Extra COCB_InsertNodeDir      ),
               
               ("term/universe"            , Builtin_Extra COCB_Uni                ),
               ("term/variable"            , Builtin_Extra COCB_Var                ),
               ("term/apply"               , Builtin_Extra COCB_Ap                 ),
               ("term/lambda"              , Builtin_Extra (COCB_Bind False Lambda )),
               ("term/forall"              , Builtin_Extra (COCB_Bind False Prod   )  ),
               ("term/mu"                  , Builtin_Extra COCB_Mu                 ),

               ("context/intro"           , Builtin_Extra COCB_Hyp                ),
               ("context/intro-before"    , Builtin_Extra COCB_HypBefore          ),
               ("context/extro-lambda"    , Builtin_Extra (COCB_Bind True Lambda  ) ),
               ("context/extro-forall"    , Builtin_Extra (COCB_Bind True Prod    )   ),
               ("context/rename"          , Builtin_Extra COCB_Rename             ),
               ("context/substitute"      , Builtin_Extra COCB_Subst              ),
               ("context/type"            , Builtin_Extra COCB_TypeOf             ),
               ("context/hypotheses"      , Builtin_Extra COCB_ContextVars        )
               ]])
  where mkDict = foldr addElt (c'map zero)
        addElt (x,v) = atP (splitPath x) %- Just v
        splitPath ('/':x) = ("",uncurry (:) (splitPath x))
        splitPath (h:t) = let ~(w,l) = splitPath t in (h:w,l)
        splitPath [] = ("",[])
        atP (h,[]) = at h
        atP (h,x:t) = at h.l'Just (StackDict zero).t'StackDict.atP (x,t)

outputComment c = runExtraState $ do outputText =~ \o t -> o (c+t)

