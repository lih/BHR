{-# LANGUAGE CPP #-}
module Graphics.Widget.Button(
  Button,button,
  ) where

import Definitive
import Graphics.Widget.Internals
import Graphics.Widget.Image
import IO.Time (currentTime)

#include "definitive-graphics.h"

data Button = Button {
  _buttonText :: Maybe String,
  _buttonImage :: Maybe (ImagePos,Widget Image),
  _buttonLastActivation :: Seconds
  }
HASPROP(LabelProp,Button,Maybe String,FIELD_LENS(_buttonText))
HASPROP(ButtonImageProp,Button,Maybe (ImagePos,Widget Image),FIELD_LENS(_buttonImage))
HASPROP(LastActivationProp,Button,Seconds,FIELD_LENS(_buttonLastActivation))

FOREIGN(gtk_button_new_with_label,Ptr CChar -> IO (Ptr GtkWidget))
FOREIGN(gtk_button_set_label,Ptr GtkWidget -> Ptr CChar -> IO ())
FOREIGN(gtk_button_set_always_show_image,Ptr GtkWidget -> CInt -> IO ())
FOREIGN(gtk_button_set_image,Ptr GtkWidget -> Ptr GtkWidget -> IO ())
FOREIGN(gtk_button_set_image_position,Ptr GtkWidget -> CInt -> IO ())
button :: Maybe String -> IO (Widget Button)
button ms = do
  h <- newGObject (case ms of
                       Just s -> withCString s gtk_button_new_with_label
                       _ -> gtk_button_new_with_label nullPtr)
  ret <- newWidget h (Button ms Nothing 0) <| do
    onChangeQual (Just "gtk") label (\_ ms' -> withForeignPtr h $ \p -> case ms' of
                      Just s -> withCString s (gtk_button_set_label p)
                      Nothing -> gtk_button_set_label p nullPtr)
    onChangeQual (Just "gtk") buttonImage $ \_ i' -> withForeignPtr h $ \p -> case i' of
      Nothing -> gtk_button_set_always_show_image p 0
      Just (pos,img) -> do
        withGtkWidget img $ \p' -> gtk_button_set_image p p'
        gtk_button_set_image_position p $ case pos of
          IP_Left -> GTK_POS_LEFT
          IP_Right -> GTK_POS_RIGHT
          IP_Top -> GTK_POS_TOP
          IP_Bottom -> GTK_POS_BOTTOM
        gtk_button_set_always_show_image p 1
  connectSignal h "clicked" =<< callback_pp_ $ \_ _ -> do
    setDynamicQual (Just "gtk") ret lastActivation =<< currentTime
  return ret
