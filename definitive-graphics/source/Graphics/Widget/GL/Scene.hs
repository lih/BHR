{-# LANGUAGE CPP #-}
module Graphics.Widget.GL.Scene where

import Definitive

import Graphics.Rendering.OpenGL.GL.BeginEnd (renderPrimitive)
import Graphics.Rendering.OpenGL.GL.CoordTrans (matrix,GLmatrix,translate,rotate,scale)
import Graphics.Rendering.OpenGL.GL.Framebuffer (clear,ClearBuffer(..))
import Graphics.Rendering.OpenGL.GL.Tensor (Vector3(..),Vertex3(..))
import Graphics.Rendering.OpenGL.GL.VertexSpec (Color4(..),TexCoord2(..),texCoord,vertex,color)
import Graphics.Widget.GL.Core
import Graphics.Widget.GL.Texture
import Graphics.Widget.GL.Vertex
#if MIN_VERSION_OpenGL(2,12,0)
import Data.StateVar (($=))
import qualified Data.StateVar as GL (get)
#else
import Graphics.Rendering.OpenGL.GL.StateVar (($=))
import qualified Graphics.Rendering.OpenGL.GL.StateVar as GL (get)
#endif
import qualified Graphics.Rendering.OpenGL.GL.PrimitiveMode as GL (PrimitiveMode(..))

type Scene t = [SceneItem t]
data SceneItem t = Shape [ShapeProp] (Shape t)
                 | SubScene [Transform t] (Scene t)
                 | Cached (IO ())
data Shape t = Polygon [Vertex t]
             | Quads [V4 (Vertex t)]
             | Triangles [V3 (Vertex t)]
             | TriangleStrip [Vertex t]
data Vertex t = Vertex [VertexProp t] !t !t !t
data VertexProp t = Color (V4 t)
                  | TexCoord (V2 t)
data ShapeProp = Texture Texture
data Transform t = Translate !t !t !t
                 | Rotate !t (V3 t)
                 | Zoom !t !t !t

instance Graphics a => Graphics [a] where draw = traverse_ draw
instance Graphics (SceneItem Coord) where 
  draw (Shape ps s) = traverse_ draw ps >> draw s
  draw (SubScene trs s) = preservingMatrix $ (traverse_ draw' (reverse trs) >> draw s)
    where draw' (Translate dx dy dz) = translate (Vector3 dx dy dz)
          draw' (Rotate a (V3 ax ay az)) = rotate a (Vector3 ax ay az)
          draw' (Zoom zx zy zz) = scale zx zy zz
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
clearScreen = clear [ColorBuffer,DepthBuffer]

i'Vertex :: Iso ([VertexProp t],t,t,t) ([VertexProp t'],t',t',t') (Vertex t) (Vertex t')
i'Vertex = iso (\(Vertex ps x y z) -> (ps,x,y,z)) (\(ps,x,y,z) -> Vertex ps x y z)

vert = Vertex []
cvert c = Vertex [c]

vProps :: Lens' (Vertex t) [VertexProp t]
vProps = i'Vertex.l'1

pQuad p a b c d = V4 a b c d <&> \c@(x,y,z) -> Vertex (p c) x y z 
pSquare p (x,y) w = pQuad p (x,y,0) (x',y,0) (x',y',0) (x,y',0)
  where x' = x+w ; y' = y+w
quad = pQuad (const [])
square = pSquare (const [])

textured = liftA2 f texMap
  where texMap = V4 (txc 0 0) (txc 1 0) (txc 1 1) (txc 0 1)
        txc = map2 TexCoord V2
        f c v = v & vProps%~(c:)

cube (x,y,z) (w,h,j) = [
  quad a b c d,
  quad a b b' a',
  quad b' b c c',
  quad d' c' c d,
  quad a a' d' d,
  quad d' c' b' a'
  ]
  where x' = x+w ; y' = y+h ; z' = z+j
        [a,a',b,b',d,d',c,c'] = liftA3 (,,) [x,x'] [y,y'] [z,z']

hsv :: (Invertible t,Invertible t',RealFrac t, RealFrac t',Ord t,Ord t') => Iso (V4 t) (V4 t') (V4 t) (V4 t')
hsv = iso toHSV fromHSV
  where toHSV (V4 r g b a) = V4 h s v a
          where cmax = r`max`g`max`b
                cmin = r`min`g`min`b
                delta = cmax - cmin
                h = (1/6)*(base + (d'/delta))
                  where (d',base) | cmax == r = (g-b,0)
                                  | cmax == g = (b-r,2)
                                  | otherwise = (r-g,4)
                s = delta / cmax
                v = cmax
        fromHSV (V4 h s v a) = V4 (r'+m) (g'+m) (b'+m) a
          where c = v*s ; scaled = floor (6*h) :: Int
                x = c * (1 - abs (fromIntegral (scaled`mod`2)-1))
                m = v * c
                (r',b',g') = case scaled of
                  0 -> (c,x,0)
                  1 -> (x,c,0)
                  2 -> (0,c,x)
                  3 -> (0,x,c)
                  4 -> (x,0,c)
                  _ -> (c,0,x)
               
rgb r g b = V4 r g b 1
grey g = rgb g g g
gray = grey
black = grey 0
white = grey 1

red = rgb 1 0 0
green = rgb 0 1 0
blue = rgb 0 0 1
yellow = green+red
magenta = red+blue
cyan = green+blue

zoom = (join.join) Zoom

