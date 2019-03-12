{-# LANGUAGE CPP #-}
module Graphics.Widget.Image (
  Image(..),IconSize(..),ImagePos(..),image
  ) where

import Definitive
import Graphics.Widget.Internals

#include "definitive-graphics.h"

data Image = Icon IconSize String
           | FileImage String
           deriving (Eq,Ord)
data ImagePos = IP_Left | IP_Right | IP_Top | IP_Bottom
              deriving (Eq,Ord,Bounded,Enum)
data IconSize = IS_Menu | IS_SmallToolbar | IS_LargeToolbar | IS_Button | IS_DragNDrop | IS_Dialog
instance Eq IconSize where a == b = fromEnum a == fromEnum b
instance Ord IconSize where compare = comparing fromEnum
instance Enum IconSize where
  fromEnum IS_Button = GTK_ICON_SIZE_BUTTON
  fromEnum IS_Menu = GTK_ICON_SIZE_MENU
  fromEnum IS_SmallToolbar = GTK_ICON_SIZE_SMALL_TOOLBAR
  fromEnum IS_LargeToolbar = GTK_ICON_SIZE_LARGE_TOOLBAR
  fromEnum IS_DragNDrop = GTK_ICON_SIZE_DND
  fromEnum IS_Dialog = GTK_ICON_SIZE_DIALOG
  toEnum GTK_ICON_SIZE_BUTTON = IS_Button
  toEnum GTK_ICON_SIZE_MENU = IS_Menu
  toEnum GTK_ICON_SIZE_SMALL_TOOLBAR = IS_SmallToolbar
  toEnum GTK_ICON_SIZE_LARGE_TOOLBAR = IS_LargeToolbar
  toEnum GTK_ICON_SIZE_DND = IS_DragNDrop
  toEnum GTK_ICON_SIZE_DIALOG = IS_Dialog
  toEnum _ = error "Invalid enum value for IconSize"

FOREIGN(gtk_image_new_from_icon_name,Ptr CChar -> CInt -> IO (Ptr GtkWidget))
FOREIGN(gtk_image_new_from_file,Ptr CChar -> IO (Ptr GtkWidget))
FOREIGN(gtk_image_set_from_icon_name,Ptr GtkWidget -> Ptr CChar -> CInt -> IO ())
FOREIGN(gtk_image_set_from_file,Ptr GtkWidget -> Ptr CChar -> IO ())
FOREIGN(gtk_event_box_new,IO (Ptr GtkWidget))
FOREIGN(gtk_container_add,Ptr GtkWidget -> Ptr GtkWidget -> IO ())
image :: Image -> IO (Widget Image)
image i = do
  h <- newGObject $ case i of
    Icon sz n -> withCString n $ \n' -> gtk_image_new_from_icon_name n' (fromIntegral (fromEnum sz))
    FileImage n -> withCString n $ \n' -> gtk_image_new_from_file n'
  h' <- newGObject gtk_event_box_new
  withForeignPtr h' $ \p -> withForeignPtr h (gtk_container_add p)
  newWidget h' i <| do onChange traitValue $ \_ i' -> withForeignPtr h $ \p -> case i' of
                         Icon sz n -> withCString n $ \n' -> gtk_image_set_from_icon_name p n' (fromIntegral (fromEnum sz))
                         FileImage n -> withCString n $ \n' -> gtk_image_set_from_file p n'
