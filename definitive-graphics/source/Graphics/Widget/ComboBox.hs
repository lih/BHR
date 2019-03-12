{-# LANGUAGE CPP, UndecidableInstances #-}
module Graphics.Widget.ComboBox(
  ComboBox,comboBox,
  ) where

import Definitive
import Graphics.Widget.Internals

#include "definitive-graphics.h"

data ComboBox = ComboBox {
  _comboBoxSelection :: Int,
  _comboBoxOptions :: [String]
  }
HASPROP(SelectedProp,ComboBox,Int,FIELD_LENS(_comboBoxSelection))
HASPROP(AlternativesProp,ComboBox,[String],FIELD_LENS(_comboBoxOptions))

FOREIGN(gtk_combo_box_text_new,IO (Ptr GtkWidget))
FOREIGN(gtk_combo_box_get_active,Ptr GtkWidget -> IO CInt)
FOREIGN(gtk_combo_box_set_active,Ptr GtkWidget -> CInt -> IO ())
FOREIGN(gtk_combo_box_text_remove_all,Ptr GtkWidget -> IO ())
FOREIGN(gtk_combo_box_text_append_text,Ptr GtkWidget -> Ptr CChar -> IO ())
comboBox :: [String] -> IO (Widget ComboBox)
comboBox a = do
  h <- newGObject gtk_combo_box_text_new
  let setAlts alts = withForeignPtr h $ \p -> do
        gtk_combo_box_text_remove_all p
        sequence_ [withCString s (gtk_combo_box_text_append_text p) | s <- alts]
  setAlts a
  ret <- newWidget h (ComboBox (-1) a) <| do
    onChangeQual (Just "gtk") selected $ \_ s' -> do
      withForeignPtr h $ \p -> gtk_combo_box_set_active p (fromIntegral s')
    onChangeQual (Just "gtk") alternatives (\_ as' -> setAlts as')
  connectSignal h "changed" =<< callback_pp_ $ \_ _ -> do
    act <- withForeignPtr h gtk_combo_box_get_active
    setDynamicQual (Just "gtk") ret selected (fromIntegral act)
  return ret
