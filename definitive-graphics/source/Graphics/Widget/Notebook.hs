{-# LANGUAGE CPP #-}
module Graphics.Widget.Notebook(
  Notebook,notebook
  ) where

import Definitive
import Graphics.Widget.Internals

#include "definitive-graphics.h"

data Notebook l w = Notebook {
  _notebookTabs :: [(Widget l,Widget w)],
  _notebookSelectedPage :: Int
  }
HASPROP(ChildrenProp,Notebook l w,[(Widget l,Widget w)],FIELD_LENS(_notebookTabs))
HASPROP(SelectedProp,Notebook l w,Int,FIELD_LENS(_notebookSelectedPage))

FOREIGN(gtk_widget_show_all,Ptr GtkWidget -> IO ())
FOREIGN(gtk_container_remove,Ptr GtkWidget -> Ptr GtkWidget -> IO ())
FOREIGN(gtk_notebook_new,IO (Ptr GtkWidget))
FOREIGN(gtk_notebook_set_current_page,Ptr GtkWidget -> CInt -> IO ())
FOREIGN(gtk_notebook_insert_page,Ptr GtkWidget -> Ptr GtkWidget -> Ptr GtkWidget -> CInt -> IO ())
notebook :: IO (Widget (Notebook l w))
notebook = do
  h <- newGObject gtk_notebook_new
  ret <- newWidget h (Notebook [] 0) <| do
    onChangeQual (Just "gtk") children $ \cs cs' -> withForeignPtr h $ \p -> do
      sequence_ [withGtkWidget c (gtk_container_remove p) | (_,c) <- cs]
      sequence_ [do withGtkWidget wl $ \l -> withGtkWidget wc $ \c -> gtk_notebook_insert_page p c l (-1)
                | (wl,wc) <- cs']
      gtk_widget_show_all p
    onChangeQual (Just "gtk") selected $ \_ s -> withForeignPtr h $ \p ->
      gtk_notebook_set_current_page p (fromIntegral s)
  connectSignal h "switch-page" =<< callback_ppip_ $ \_ _ i _ ->
    setDynamicQual (Just "gtk") ret selected (fromIntegral i)
  return ret
