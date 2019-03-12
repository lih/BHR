{-# LANGUAGE CPP #-}
module Graphics.Widget.TextInput(TextInput,textInput) where

import Definitive
import Graphics.Widget.Internals
import IO.Time(currentTime)

#include "definitive-graphics.h"

data TextInput = TextInput {
  _entryLastTextChange :: Seconds,
  _entryCursor :: Int,
  _entryText :: (String,String)
  }
HASPROP(LabelProp,TextInput,String,
  lens (\i -> let (h,t) = _entryText i in reverse h + t) (\i t -> i { _entryText = ([],t), _entryCursor = 0 }))
HASPROP(LastTextChangeProp,TextInput,Seconds,FIELD_LENS(_entryLastTextChange))
HASPROP(InputTextProp,TextInput,(String,String),FIELD_LENS(_entryText))
HASPROP(CursorProp,TextInput,Int,FIELD_LENS(_entryCursor))

FOREIGN(gtk_entry_new,IO (Ptr GtkWidget))
textInput :: IO (Widget TextInput)
textInput = do
  h <- newGObject gtk_entry_new
  ret <- newWidget h (TextInput 0 zero zero)
  connectSignal h "delete-text" =<< callback_piip_ $ \_ start end _ -> void $ do
    let count = fromIntegral (end - start)
        c = fromIntegral start
    tm <- currentTime
    pure ret <| do lastTextChange =- tm
                   oldc <- cursor `swapWith` const (fromIntegral c)
                   inputText =~ \(h,t) -> if oldc>c then let (hh,ht) = splitAt (oldc-c) h in (ht, drop count (reverse hh+t))
                                          else let (th,tt) = splitAt (c-oldc) t in (reverse th+h,drop count tt)
  connectSignal h "insert-text" =<< callback_ppipp_ $ \_ txt txtl posp _ -> void $ do
    c <- fromCInt <$> peek posp
    str <- peekCStringLen (txt,fromIntegral txtl)
    tm <- currentTime
    return ret <| do lastTextChange =- tm
                     oldc <- cursor `swapWith` const (c+length str)
                     inputText =~ \(h,t) -> if oldc>c then let (hh,ht) = splitAt (oldc-c) h in (reverse str+ht,reverse hh+t)
                                            else let (th,tt) = splitAt (c-oldc) t in (reverse str+reverse th+h,tt)
  return ret

