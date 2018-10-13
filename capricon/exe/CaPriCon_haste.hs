{-# LANGUAGE CPP, RebindableSyntax, ViewPatterns, TupleSections, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, LambdaCase, TypeOperators, RankNTypes, GeneralizedNewtypeDeriving, TypeFamilies, NoMonomorphismRestriction #-}
module Main where

import Definitive
import Language.Parser
import Algebra.Monad.Concatenative
import System.IO (openFile,hIsTerminalDevice,IOMode(..),hClose)
import System.Environment (getArgs,lookupEnv)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.IORef
import Data.CaPriCon
import CaPriCon.Run
import qualified Haste.Foreign as JS
import qualified Haste as JS
import qualified Haste.DOM as JS
import qualified Haste.Events as JS

runComment c = unit

main :: IO ()
main = do
  let runWords r ws = do
        st <- readIORef r
        (st',v) <- runWordsState ws st
        writeIORef r st'
        return v
      runWordsState ws st = ($st) $ from (stateT.concatT) $^ do
        foldr (\w tl -> do
                  x <- runExtraState (getl endState)
                  unless x $ do execSymbol runCOCBuiltin runComment w; tl) unit ws
        out <- runExtraState (outputText <~ \x -> (id,x))
        return (out "")
      withSubElem root cl = JS.withElemsQS root ('.':cl) . traverse_
      withSubElems _ [] k = k []
      withSubElems root (h:t) k = withSubElem root h $ \h' -> withSubElems root t $ \t' -> k (h':t')
  
  prelude <- JS.withElem "capricon-prelude" (\e -> JS.getProp e "textContent")
  -- JS.alert $ JS.toJSString prelude
  (initState,_) <- runWordsState (stringWords prelude) (defaultState (cocDict "0.7") (COCState False [] zero id))
  
  roots <- JS.elemsByClass "capricon-steps"
  (\k -> foldr k (const unit) roots initState) $ \root next state -> do
    withSubElems root ["capricon-input","capricon-submit","capricon-output"] $ \[inp,sub,out] -> do
      JS.withElemsQS root ".capricon-context" $ \case
        [con] -> do
          context <- JS.getProp con "textContent"
          -- JS.alert $ JS.toJSString context
          (state',_) <- runWordsState (stringWords context) state
          JS.onEvent sub JS.Click $ \_ -> do
            Just v <- JS.getValue inp
            (_,x) <- runWordsState (stringWords v) state'
            JS.setProp out "textContent" x
          next state'
        
  return ()
