{-# LANGUAGE CPP #-}
module Graphics.Widget.Terminal(
  Terminal(..),terminal
  ) where

import Definitive
import Graphics.Widget.Internals

#include "definitive-graphics.h"

data Terminal = IdleTerminal
              | CommandTerminal (String,[String])
              deriving (Eq,Ord)
HASPROP(TerminalStatusProp,Terminal,Terminal,id)
                
FOREIGN(vte_terminal_new,IO (Ptr GtkWidget))
foreign import ccall "definitive_vte_terminal_fork_command"
  vte_terminal_fork_command :: Ptr GtkWidget -> Ptr (Ptr CChar) -> IO CInt
terminal :: IO (Widget Terminal)
terminal = do
  h <- newGObject vte_terminal_new
  ret <- newWidget h IdleTerminal <| do
    onChangeQual (Just "gtk") terminalStatus $ \_ st -> case st of
      CommandTerminal (prog,args) -> void $ do
        withForeignPtr h $ \p -> withCStrings (prog:args) $ \ps -> withArray0 nullPtr ps $ vte_terminal_fork_command p
      IdleTerminal -> unit
  connectSignal h "child-exited" =<< callback_pip_ $ \_ _ _ -> do
    setDynamicQual (Just "gtk") ret terminalStatus IdleTerminal
  return ret
