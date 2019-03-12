{-# LANGUAGE CPP #-}
module Graphics.Widget.GLArea(
  GLArea,glArea,
  ) where

import Definitive
import Graphics.Widget.Internals
import Graphics.Widget.GL.Core
import Graphics.Widget.GL.Scene

#include "definitive-graphics.h"

data GLArea = GLArea {
  _lastFrame :: Assoc Seconds (Scene Coord)
  }
HASPROP(LastFrameProp,GLArea,Assoc Seconds (Scene Coord),FIELD_LENS(_lastFrame))

FOREIGN(gtk_gl_area_new,IO (Ptr GtkWidget))
FOREIGN(gtk_gl_area_queue_render,Ptr GtkWidget -> IO ())
glArea :: Scene Coord -> IO (Widget GLArea)
glArea sc = do
  h <- newGObject gtk_gl_area_new
  ret <- newWidget h (GLArea (Assoc 0 sc)) <| do
    onChange lastFrame $ \_ _ -> withForeignPtr h gtk_gl_area_queue_render
  connectSignal h "render" =<< callback_pppb $ \_ _ _ -> do
    putStrLn "Rendering..."
    Assoc _ sc <- getDynamic ret lastFrame
    draw sc
    return False
  return ret
      
