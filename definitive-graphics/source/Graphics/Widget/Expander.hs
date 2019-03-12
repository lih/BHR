{-# LANGUAGE CPP #-}
module Graphics.Widget.Expander(
  Expander,expander,
  ) where

import Definitive
import Graphics.Widget.Internals

#include "definitive-graphics.h"

data Expander w = Expander {
  _expanderExpanded :: Bool,
  _expanderLabel :: String,
  _expanderChild :: Widget w
  }
HASPROP(ChildProp,Expander t,Widget t,FIELD_LENS(_expanderChild))
HASPROP(LabelProp,Expander w,String,FIELD_LENS(_expanderLabel))
HASPROP(ExpandedProp,Expander w,Bool,FIELD_LENS(_expanderExpanded))

FOREIGN(gtk_container_add,Ptr GtkWidget -> Ptr GtkWidget -> IO ())
FOREIGN(gtk_container_remove,Ptr GtkWidget -> Ptr GtkWidget -> IO ())
FOREIGN(gtk_expander_new,Ptr CChar -> IO (Ptr GtkWidget))
FOREIGN(gtk_expander_set_label,Ptr GtkWidget -> Ptr CChar -> IO ())
FOREIGN(gtk_expander_get_expanded,Ptr GtkWidget -> IO CInt)
FOREIGN(gtk_expander_set_expanded,Ptr GtkWidget -> CInt -> IO ())
FOREIGN(gtk_widget_show_all,Ptr GtkWidget -> IO ())
expander :: String -> Widget a -> IO (Widget (Expander a))
expander l w = do
  h <- newGObject (withCString l gtk_expander_new)
  withForeignPtr h (\p -> withGtkWidget w (gtk_container_add p))
  ret <- newWidget h (Expander False l w) <| do
    onChangeQual (Just "gtk") label (\_ l' -> withForeignPtr h $ \p -> withCString l' (gtk_expander_set_label p))
    onChangeQual (Just "gtk") child (\c c' -> withForeignPtr h $ \p -> do
                                         withGtkWidget c (gtk_container_remove p)
                                         withGtkWidget c' (gtk_container_add p)
                                         gtk_widget_show_all p)
    onChangeQual (Just "gtk") expanded (\_ e' -> withForeignPtr h $ \p -> gtk_expander_set_expanded p (if e' then 1 else 0))
  connectSignal h "activate" =<< callback_pp_ $ \_ _ -> do
    e <- withForeignPtr h gtk_expander_get_expanded
    setDynamicQual (Just "gtk") ret expanded (e==0)
  return ret

