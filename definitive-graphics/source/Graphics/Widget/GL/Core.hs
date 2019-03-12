{-# LANGUAGE CPP #-}
module Graphics.Widget.GL.Core where

import Definitive
#if MIN_VERSION_OpenGLRaw(3,0,0)
import Graphics.GL.Types (GLfloat)
#else
import Graphics.Rendering.OpenGL.Raw.Types (GLfloat)
#endif

type Coord = GLfloat
class Graphics g where
  draw :: g -> IO ()
