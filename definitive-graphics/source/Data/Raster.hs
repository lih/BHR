{-# LANGUAGE TypeOperators #-}
module Data.Raster (
  Raster,PNG,BMP,JPG,bmp,jpg,png,raster,
  ) where

import Definitive
import Language.Format
import Codec.Picture

data Raster = Raster DynamicImage
newtype JPG = JPG Raster
newtype PNG = PNG Raster
newtype BMP = BMP Raster

jpg :: Raster:<->:JPG
jpg = iso JPG (\(JPG r) -> r)
bmp :: Raster:<->:BMP
bmp = iso BMP (\(BMP r) -> r)
png :: Raster:<->:PNG
png = iso PNG (\(PNG r) -> r)

instance Serializable Bytes JPG where
  encode _ (yb (raster.jpg) -> ImageYCbCr8 i) = encodeJpeg i^.bytesBuilder
  encode _ _ = error "Unsupported image format for JPeg encoding"
instance Format Bytes JPG where
  datum = convert.decodeJpeg.by chunk^.mapping (mapping (raster.jpg.pureWriter)).parser
instance Serializable Bytes PNG where
  encode _ (yb (raster.png) -> i) = (error<|>by bytesBuilder) (encodeDynamicPng i)
instance Format Bytes PNG where
  datum = convert.decodePng.by chunk^.mapping (mapping (raster.png.pureWriter)).parser
instance Serializable Bytes BMP where
  encode _ (yb (raster.bmp) -> i) = (error<|>by bytesBuilder) (encodeDynamicBitmap i)
instance Format Bytes BMP where
  datum = convert.decodeBitmap.by chunk^.mapping (mapping (raster.bmp.pureWriter)).parser
     
raster :: DynamicImage:<->:Raster
raster = iso Raster (\(Raster i) -> i)


