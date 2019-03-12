{-# LANGUAGE RankNTypes #-}
module Graphics.Widget.GL.Texture.Core (
  Texture,
  texture,texture',readTexture,readTextures,readTextures'
  ) where

import Definitive
import Language.Format
import Codec.Picture
import Codec.Picture.Types (convertImage)
import qualified Graphics.Rendering.OpenGL as GL
import Data.Raster
import Graphics.Widget.GL.Core

-- |The abstract Texture type
data Texture = Texture GL.TextureObject
             deriving Show

data TextureFormat = RGB | RGBA | Greyscale | GreyscaleA

pixelFormats RGBA = (GL.RGBA',GL.RGBA)
pixelFormats RGB = (GL.RGB',GL.RGB)
pixelFormats Greyscale = (GL.SLuminance8,GL.Luminance)
pixelFormats GreyscaleA = (GL.SLuminance8Alpha8,GL.LuminanceAlpha)

-- |Convert a Raster to a texture
texture (yb raster -> i) = by thunk $ yb eitherT $ do
  let conv (ImageRGB8 (Image w h d))  = return (pixelFormats RGB,w,h,d)
      conv (ImageY8 (Image w h d))    = return (pixelFormats Greyscale,w,h,d)
      conv (ImageRGBA8 (Image w h d)) = return (pixelFormats RGBA,w,h,d)
      conv (ImageYA8 (Image w h d))   = return (pixelFormats GreyscaleA,w,h,d)
      conv (ImageYCbCr8 i)            = conv (ImageRGB8 (convertImage i))
      conv _                          = throw "Unhandled image format"

  ((f,f'),w,h,d) <- by eitherT $ pure $ conv i 

  lift $ do
    [tex] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D GL.$= Just tex
    unsafeWith d $ GL.build2DMipmaps GL.Texture2D f (fromIntegral w) (fromIntegral h)
      . GL.PixelData f' GL.UnsignedByte
    GL.textureFilter  GL.Texture2D GL.$= ((GL.Linear', Just GL.Nearest), GL.Linear')
    GL.textureFunction GL.$= GL.Modulate
    return (Texture tex)
texture' = (error<|>id).texture

listToEither [] = Left zero
listToEither (~(_,a):_) = Right a

-- |Read a texture from a file.
readTexture name = (readBytes name <&> joinMap texture . listToEither . yb parser image)
  where image = yb jpg<$>datum
                <+>yb png<$>datum
                <+>yb bmp<$>datum
-- |Try to read a structure of files into a structure of textures.
readTextures = map sequence . traverse readTexture
-- |Read a structure of files into a structure of textures, raising an error
-- if it fails.
readTextures' = map2 (error<|>id) readTextures

instance Graphics Texture where
  draw (Texture t) = GL.textureBinding GL.Texture2D GL.$= Just t

