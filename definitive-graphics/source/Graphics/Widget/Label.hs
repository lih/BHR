{-# LANGUAGE CPP #-}
module Graphics.Widget.Label(Text,text,AccelText,accelText) where

import Definitive
import Graphics.Widget.Internals

#include "definitive-graphics.h"

data Text = Text {
  _labelText :: String
  }
newtype AccelText = AccelText {
  _accelText :: (String,Maybe String)
  }
HASPROP(LabelProp,Text,String,FIELD_LENS(_labelText))
HASPROP(LabelProp,AccelText,(String,Maybe String),FIELD_LENS(_accelText))

FOREIGN(gtk_label_new,Ptr CChar -> IO (Ptr GtkWidget))
FOREIGN(gtk_label_set_text,Ptr GtkWidget -> Ptr CChar -> IO ())
FOREIGN(gtk_event_box_new,IO (Ptr GtkWidget))
FOREIGN(gtk_container_add,Ptr GtkWidget -> Ptr GtkWidget -> IO ())
text :: String -> IO (Widget Text)
text s = withCString s $ \s' -> do
  h <- newGObject (gtk_label_new s')
  h' <- newGObject gtk_event_box_new
  withForeignPtr h' $ \p -> withForeignPtr h (gtk_container_add p)
  newWidget h' (Text s)
    <| onChange label (\_ s -> withForeignPtr h $ withCString s . gtk_label_set_text)

FOREIGN(gtk_accel_label_new,Ptr CChar -> IO (Ptr GtkWidget))
FOREIGN(gtk_accel_label_set_accel,Ptr GtkWidget -> CInt -> CInt -> IO ())
FOREIGN(gtk_label_set_xalign,Ptr GtkWidget -> CFloat -> IO ())
accelText :: String -> IO (Widget AccelText)
accelText text = do
  h <- newGObject $ withCString text gtk_accel_label_new <*= \p -> do
    gtk_label_set_xalign p 0
  newWidget h (AccelText (text,Nothing)) <| do
    onChange (label.l'1) $ \_ text' -> withForeignPtr h $ \p -> withCString text' (gtk_label_set_text p)
    onChange (label.l'2) $ \_ acc' -> withForeignPtr h $ \p -> do
      (key,mods) <- case acc' of
        Just a -> parseAccelerator a
        Nothing -> return (0,0)
      gtk_accel_label_set_accel p key mods
