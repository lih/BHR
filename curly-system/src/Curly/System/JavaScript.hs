{-# LANGUAGE RecursiveDo #-}
module Curly.System.JavaScript(system,systemASM,generateJS) where

import Definitive
import Language.Format
import Curly.Core
import Curly.Core.Annotated
import Curly.Core.Library
import Curly.System.Base

newtype Instruction = Instruction String

strEncode :: String -> Builder
strEncode = foldMap encode
instance BCSerializable Instruction where
  bcEncode (Instruction s) = BC 1 1 (foldMap encode (s+";")^..bytesBuilder)

system = System "javascript" id (Standalone (void . rawProgram [TextSection])) Nothing
         (RawSystem (yb bytesBuilder . strEncode . generateJS . anonymous . by leafVal))
systemASM =
  System "jsasm" id 
  (Standalone
   (\m -> rawProgram [RawSection "header",TextSection,RawSection "footer"] $ mdo
       inSection (RawSection "header") $ do
         tell $ bytesCode (Just 0,0) $ yb bytesBuilder $ strEncode (format "function main(){var %s,pc=%s;while(true){switch(pc){" (intercalate "," (map (showReg . RegID) [0..3])) (showAddr start))
       start <- m
       inSection (RawSection "footer") $ do
         tell $ bytesCode (Just 0,0) $ yb bytesBuilder $ strEncode "}}}"))
  Nothing
  $ Imperative (const js_machine)

js_machine = VonNeumannMachine {
    _destReg = RegID 0, _thisReg = RegID 1, _tmpReg = RegID 2,
    _newFunction = const getCounter,
    _cp = \l v -> instr (format "%s=%s" (showLoc l) (showVal v)),
    _add = \l v -> instr (format "%s+=%s" (showLoc l) (showVal v)),
    _load = \l a -> instr (format "%s=data[%s]" (showLoc l) (showAddr a)),
    _store = \a v -> instr (format "data[%s]=%s" (showAddr a) (showVal v)),
    _push = \v -> instr (format "stack={'val':%s,'next':stack}" (showVal v)),
    _pop = \x -> case x of
      Left n -> instr (format (intercalate ";" (take n (repeat "stack=stack.next"))))
      Right l -> instr (format "%s=stack.val;stack=stack.next" (showLoc l)),
    _pushThunk = \l -> instr (format "%s={'next':%s}" (showLoc l) (showLoc l)),
    _popThunk = \l -> instr (format "%s=%s.next" (showLoc l) (showLoc l)),
    _jcmp = \_ cmp a b addr -> instr (format "if(%s%s%s)continue i%s" (showVal a) (showCmp cmp) (showVal b) (showAddr addr)),
    _jmp = \v -> instr (showJmp v),
    _call = \v -> void $ mfix $ \next -> do
      instr (format "/* call */ stack={'val':%s,'next':stack};%s" (showAddr next) (showJmp v))
      getCounter,
    _ret = instr "/* ret */ pc=stack.val;stack=stack.next;break",
    _curlyBuiltin = js_curlyBuiltin,
    _assemblyMachine = Nothing
  }

js_curlyBuiltin :: BUILTIN_INSTR
js_curlyBuiltin x = let ?sys = js_machine in
  commonBuiltin x +
  case x of
    B_Write -> Just $ map (,Constant 0) $ defBuiltinGet TextSection "write" $ do
      [file,str,cont] <- builtinArgs 3
      pushing [thisReg] $ callThunk file
      pushing [thisReg] $ callThunk str
      instr (format "if (%s == 1) { console.log(%s); }" (showLoc (file!ValueOffset)) (showLoc (destReg!ValueOffset)))
      tailCall cont
    B_String s -> Just $ map (,Constant 0) $ defBuiltinGet TextSection ("str:"+s) $ do
      cst <- getConstantFun
      destReg!TypeOffset <-- cst
      instr (format "%s=%s" (showLoc (destReg!ValueOffset)) (show s))
      ret
    _ -> Nothing

showJmp :: Value -> String
showJmp (Constant c) = format "continue i%s" (show c)
showJmp v = format "pc=%s;break" (showVal v)

showAddr (BA a) = show a

showCmp (True,EQ) = "=="
showCmp (False,EQ) = "!="
showCmp (True,GT) = ">"
showCmp (False,GT) = "<="
showCmp (True,LT) = "<"
showCmp (False,LT) = ">="

showReg (RegID 0) = "dest"
showReg (RegID 1) = "thunk"
showReg (RegID 2) = "tmp"
showReg (RegID 3) = "extra"
showReg (RegID n) = error ("unknown register "+show n)

showOff NoStride (Offset o) = format "[%d]" o
showOff (ByteStride r) (Offset o) = format "[%s+%d]" (showReg r) o
showOff (WordStride r) (Offset o) = format "[4*%s+%d]" (showReg r) o
showOff _ ValueOffset = ".value"
showOff _ TypeOffset = ".type"
showOff _ EnvOffset = ".child"

showLoc (Register r) = showReg r
showLoc (AtOffset r i off) = showLoc r+showOff i off

showVal (Constant i) = show i
showVal (Variable l) = showLoc l

instr :: MonadASM m s => String -> m ()
instr s = do
  pos <- getCounter
  tell (bcEncode (Instruction $ format "case %s:i%s:%s" (showAddr pos) (showAddr pos) s)) 

generateJS :: Semantic e i (Symbol s) => e -> String
generateJS = generate 0
  where generate n e = case sem e of
          SemSymbol (Argument n') -> "x"+show (n-n'-1)
          SemSymbol (Builtin _ b) -> pretty b
          SemAbstract _ e' -> format "(x%s => %s)" (show n) (generate (n+1) e')
          SemApply f x -> format "%s(%s)" (generate n f) (generate n x)
