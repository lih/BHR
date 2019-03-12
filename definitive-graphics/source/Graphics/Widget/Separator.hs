{-# LANGUAGE CPP #-}
module Graphics.Widget.Separator(
  Separator,separator,Frame,frame,textFrame
  ) where

import Definitive
import Graphics.Widget.Internals
import Graphics.Widget.Containers.Internal
import Graphics.Widget.Label

#include "definitive-graphics.h"

data Separator = Separator 

FOREIGN(gtk_separator_new, CInt -> IO (Ptr GtkWidget))
separator :: Orientation -> IO (Widget Separator)
separator o = do
  h <- newGObject $ gtk_separator_new $ case o of Horizontal -> GTK_ORIENTATION_HORIZONTAL
                                                  Vertical -> GTK_ORIENTATION_VERTICAL
  newWidget h Separator

data Frame l a = Frame {
  _frameLabel :: Widget l,
  _frameContents :: Widget a
  }
HASPROP(ChildProp,Frame l a,Widget a,FIELD_LENS(_frameContents))
HASPROP(LabelProp,Frame l a,Widget l,FIELD_LENS(_frameLabel))

FOREIGN(gtk_frame_new,Ptr CChar -> IO (Ptr GtkWidget))
FOREIGN(gtk_frame_set_label_widget,Ptr GtkWidget -> Ptr GtkWidget -> IO ())
frame :: Widget l -> Widget a -> IO (Widget (Frame l a))
frame l a = do
  h <- newGObject (gtk_frame_new nullPtr)
  let setLabel x = withForeignPtr h $ \ph -> withGtkWidget x (gtk_frame_set_label_widget ph)
      setContents x = withForeignPtr h $ \ph -> withGtkWidget x (gtk_container_add ph)
  setLabel l ; setContents a
  newWidget h (Frame l a) <| do
    onChange label $ \_ -> setLabel
    onChange child $ \c c' -> withForeignPtr h $ \ph -> do
      withGtkWidget c (gtk_container_remove ph)
      withGtkWidget c' (gtk_container_add ph)

textFrame :: String -> Widget a -> IO (Widget (Frame Text a))
textFrame s w = text s >>= \l -> frame l w
