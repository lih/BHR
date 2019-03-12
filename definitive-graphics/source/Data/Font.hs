{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving #-}
module Data.Font (
  Font,
  font,glyph,glyph'
  ) where

import Definitive
import Data.Raster
import Graphics.Rendering.TrueType.STB hiding (Font,findGlyph)
import qualified Graphics.Rendering.TrueType.STB as TT
import Control.Exception (SomeException)
import qualified Data.Vector.Storable as V
import Codec.Picture

newtype Font = Font [TT.Font]
             deriving (Semigroup,Monoid)
                   
font :: Chunk -> Either SomeException Font
font s = by thunk $ catch (return . Left) $ Right . Font <$> do
  let tt = TrueType s
  enumerateFonts tt >>= traverse (initFont tt)

glyph :: Font -> (Char,Float) -> Maybe Raster
glyph (Font fs) = \(c,sc) -> foldMap (charBM c sc) fs
  where charBM c sc f = fg f c <&> \g -> bm  f g (scl sc,scl sc) & bitmap2Image
          where scl = scaleForPixelHeight (getFontVerticalMetrics f^.thunk)
        fg = map2 (by thunk) TT.findGlyph
        bm = map3 (fst.by thunk) TT.newGlyphBitmap
glyph' :: Font -> (Char,Float) -> Raster
glyph' = map2 (maybe undefined id) glyph
bitmap2Image :: Bitmap -> Raster
bitmap2Image (Bitmap (w,h) p) = ImageY8 (Image w h v')^.raster
  where v' = V.concat (reverse $ zipWith mkSlice posts (tail posts))
        v = V.unsafeFromForeignPtr0 p (w*h)
        posts = [0,w..V.length v]
        mkSlice a b = V.slice a (b-a) v
