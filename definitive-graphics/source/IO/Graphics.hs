{-# LANGUAGE CPP, FlexibleInstances, ViewPatterns, ImplicitParams, RankNTypes #-}
module IO.Graphics (
  module Data.Reactive,module Definitive,module Graphics.Widget.GL.Vertex,module Graphics.Widget.GL.Scene,module Data.Input.Button,
  
  -- * Creating windows & Handling events
  EventHandler,Coord,Position,Title,Size(..),
  spawnWindow,
  
  -- * Examining and modifying the window
  GettableStateVar,SettableStateVar,
    
  -- * Drawing the scene
  drawScene,
  ) where

import Definitive
import Data.Reactive 
import Graphics.Widget.GL.Scene
import Data.Input.Button
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.GL.Texturing.Application as GL (texture)
import qualified Graphics.Rendering.OpenGL.GL.CoordTrans as GL (Position(..))
import qualified Graphics.Rendering.OpenGL.GL.BeginEnd as GL (PrimitiveMode(..))
import qualified Graphics.Rendering.OpenGL.GL.StateVar as GL (get)
import Graphics.Rendering.OpenGL.Raw.Core31.Types (GLdouble,GLfloat)
import Graphics.Rendering.OpenGL.GL.StateVar (($=),GettableStateVar,SettableStateVar)
#if MIN_VERSION_OpenGL(2,9,0)
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (TextureTarget2D(Texture2D))
#else
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (TextureTarget(Texture2D))
#endif
import Graphics.Rendering.OpenGL.GL.VertexArrays (Capability(Enabled))
import Graphics.Rendering.OpenGL.GL.Texturing.Parameters (generateMipmap)
import Graphics.Rendering.OpenGL.GL.Colors (shadeModel,ShadingModel(Smooth))
import Graphics.Rendering.OpenGL.GL.LineSegments (lineSmooth)
import Graphics.Rendering.OpenGL.GL.PerFragment (blend,blendFunc,BlendingFactor(..),depthFunc,ComparisonFunction(..))
import Graphics.Rendering.OpenGL.GL.LineSegments (lineWidth)
import Graphics.Rendering.OpenGL.GL.Framebuffer (clearColor,clear,ClearBuffer(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec (Color4(..),TexCoord2(..),texCoord,vertex,color)
import Graphics.Rendering.OpenGL.GL.CoordTrans (Size(..),viewport,matrix,matrixMode,MatrixMode(Modelview,Projection),GLmatrix,loadIdentity,translate,rotate,scale)
import Graphics.Rendering.OpenGL.GLU.Matrix (perspective)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3(..),Vertex3(..))
import Graphics.Rendering.OpenGL.GL.BeginEnd (renderPrimitive)
import Control.Concurrent
import Graphics.Widget.GL.Vertex

type Coord = GLfloat
type Position = V2 Coord

instance Semigroup GLdouble
instance Monoid GLdouble
instance Disjonctive GLdouble
instance Semiring GLdouble
instance Ring GLdouble
instance Invertible GLdouble

instance Semigroup Coord
instance Monoid Coord
instance Disjonctive Coord
instance Semiring Coord
instance Ring Coord
instance Invertible Coord

-- |A function from the a window's inputs to a frame event
type EventHandler = ( ?mousePosition :: Event Seconds Position
                    , ?buttonChanges :: Event Seconds (Button,ButtonState) ) => IO (Event Seconds (IO ()))
type Title = String

-- |Create an OpenGL window and sinks all events into the given handler. 
spawnWindow :: ( ?birthTime :: Seconds ) => Title -> EventHandler -> IO ()
spawnWindow title sink = between GLFW.initialize GLFW.terminate $ between openWindow GLFW.closeWindow $ do
  -- This code was partially stolen from http://www.haskell.org/haskellwiki/GLFW
  sizes <- newChan
  refreshes <- newChan
  
  -- open window
  GLFW.windowTitle $= title

  -- enable 2D texturing
  GL.texture Texture2D $= Enabled 
  generateMipmap Texture2D $= Enabled
  
  shadeModel $= Smooth
  -- enable antialiasing
  lineSmooth $= Enabled
  blend      $= Enabled
  blendFunc  $= (SrcAlpha, OneMinusSrcAlpha)
  lineWidth  $= 1.5
  -- The clear color is black
  clearColor $= Color4 0 0 0 0
  -- enable the depth buffer for correct 3D rendering
  depthFunc $= Just Less

  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(Size w h) -> do
    writeChan sizes size
    viewport $= (GL.Position 0 0, size)
    between (matrixMode $= Projection) (matrixMode $= Modelview 0) $ do
      loadIdentity
      let aov = 45 -- the angle of view
          ratio = fromIntegral w/fromIntegral h
      perspective aov ratio 0.1 100
      translate (Vector3 0 (0::GLdouble) (-max (1/ratio) 1/tan (aov*pi/360)))

  GLFW.windowRefreshCallback $= writeChan refreshes ()
    
  -- Now we define our own little stuff
  -- It doesn't seem very logical to poll events when swapping buffers. 
  GLFW.disableSpecial GLFW.AutoPollEvent
  let callbackE c f = do
        ch <- newChan
        c $= f (writeChan ch)
        event (readChan ch)
  _keyboard <- callbackE GLFW.keyCallback curry
  _mousePos <- callbackE GLFW.mousePosCallback (promap (\(GL.Position x y) -> V2 x y))
  _mouseButton <- callbackE GLFW.mouseButtonCallback curry
  _size <- event (readChan sizes)
  t'refresh <- event (readChan refreshes)
  let _button = (l'1%~fromK<$>_keyboard) + (l'1%~MouseButton<$>_mouseButton)
      fromK (GLFW.CharKey c) = CharKey c
      fromK (GLFW.SpecialKey k) = SpecialKey k
      relative (Size w h) (V2 x y) = V2 ((x'-xc)/m) ((yc-y')/m)
        where [x',y',w',h'] = realToFrac<$>[x,y,w,h]
              xc = w'/2 ; yc = h'/2 ; m = min w' h'/2
  
  ev <- let ?mousePosition = relative<$>Reactive (Size 1 1) _size<|*>_mousePos
            ?buttonChanges = _button
        in sink

  -- GLFW doesn't handle multithreading nicely, so we have to
  -- manually interleave polling events and rendering within
  -- the main thread. And since waiting for events may block
  -- while the rendering might occur, we have to poll regularly.
  -- I chose a 15ms polling period because it was both reactive
  -- enough for keyboard and mouse events and not too heavy on
  -- the CPU. 
  let period = ms 15
      poll = GLFW.pollEvents <$ atTimes [?birthTime,?birthTime+period..]
  try unit $ realize $ realtime (ev + (const<$>Reactive unit ev<|*>(void _size+t'refresh))) + poll
  where openWindow = GLFW.openWindow (Size 400 400) [GLFW.DisplayDepthBits 16,GLFW.DisplayAlphaBits 8] GLFW.Window

clearScreen = clear [ColorBuffer,DepthBuffer]

instance Graphics (Widget Coord) where 
  draw (Shape ps s) = traverse_ draw ps >> draw s
  draw (SubScene trs s) = preservingMatrix $ (traverse_ draw (reverse trs) >> drawScene s)
    where draw (Translate dx dy dz) = translate (Vector3 dx dy dz)
          draw (Rotate a (V3 ax ay az)) = rotate a (Vector3 ax ay az)
          draw (Zoom zx zy zz) = scale zx zy zz
  draw (Cached c) = c
instance Graphics (Shape Coord) where
  draw (Polygon p) = renderPrimitive GL.Polygon $ traverse_ draw p
  draw (Quads sqs) = renderPrimitive GL.Quads $ traverse_ draw (Compose sqs)
  draw (Triangles trs) = renderPrimitive GL.Triangles $ traverse_ draw (Compose trs)
  draw (TriangleStrip s) = renderPrimitive GL.TriangleStrip $ traverse_ draw s
instance Graphics (Vertex Coord) where
  draw (Vertex ps x y z) = traverse_ draw ps >> vertex (Vertex3 x y z)
instance Graphics (VertexProp Coord) where
  draw (Color (V4 r g b a)) = color (Color4 r g b a)
  draw (TexCoord (V2 x y)) = texCoord (TexCoord2 x y)
instance Graphics ShapeProp where
  draw (Texture t) = draw t

withMatrix f = GL.get (matrix Nothing) >>= f
preservingMatrix ma = withMatrix $ \old ->
  ma <* (matrix Nothing $= (old :: GLmatrix Coord))

drawScene :: Scene Coord -> IO ()
drawScene = between clearScreen GLFW.swapBuffers . traverse_ draw

instance Functor Vector3 where
  map f (Vector3 x y z) = Vector3 (f x) (f y) (f z)
instance Unit Vector3 where pure = join (join Vector3)
instance Applicative Vector3 where
  Vector3 fx fy fz <*> Vector3 x y z = Vector3 (fx x) (fy y) (fz z)

instance Functor Vertex3 where
  map f (Vertex3 x y z) = Vertex3 (f x) (f y) (f z)
instance Unit Vertex3 where pure = join (join Vertex3)
instance Applicative Vertex3 where
  Vertex3 fx fy fz <*> Vertex3 x y z = Vertex3 (fx x) (fy y) (fz z)
instance Traversable Vertex3 where
  sequence (Vertex3 a b c) = liftA3 Vertex3 a b c

