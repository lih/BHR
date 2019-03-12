{-# LANGUAGE CPP #-}
module Graphics.Widget.Containers.Internal(
    newGtkBox,boxAddWidget,swapChild,Orientation(..),
    gtk_container_add,gtk_container_remove,gtk_widget_show_all
    ) where

import Definitive
import Graphics.Widget.Internals

#include "definitive-graphics.h"

data Orientation = Vertical | Horizontal

FOREIGN(gtk_box_new,CInt -> IO (Ptr GtkWidget))
newGtkBox Horizontal = gtk_box_new GTK_ORIENTATION_HORIZONTAL
newGtkBox Vertical = gtk_box_new GTK_ORIENTATION_VERTICAL

FOREIGN(gtk_widget_show_all,Ptr GtkWidget -> IO ())
FOREIGN(gtk_container_add,Ptr GtkWidget -> Ptr GtkWidget -> IO ())
FOREIGN(gtk_box_reorder_child,Ptr GtkWidget -> Ptr GtkWidget -> CInt -> IO ())
FOREIGN(gtk_container_remove,Ptr GtkWidget -> Ptr GtkWidget -> IO ())
FOREIGN(gtk_box_set_child_packing,Ptr GtkWidget -> Ptr GtkWidget -> Bool -> Bool -> CInt -> CInt -> IO ())
boxAddWidget :: ForeignPtr GtkWidget -> Int -> SubWidget b -> IO ()
boxAddWidget b i w = withForeignPtr b $ \bp -> withGtkWidget w $ \wp -> do
  (e,anch,pad) <- getDynamic w packing
  gtk_container_add bp wp
  gtk_box_reorder_child bp wp (fromIntegral i)
  gtk_box_set_child_packing bp wp
    (case e of Fitting -> False ; _ -> True) (case e of Expanding -> True ; _ -> False)
    (fromIntegral pad)
    (if anch then GTK_PACK_END else GTK_PACK_START)
swapChild :: ForeignPtr GtkWidget -> Int -> SubWidget a -> SubWidget a -> IO ()
swapChild p n a b = do
  withForeignPtr p $ withGtkWidget a . gtk_container_remove
  boxAddWidget p n b
