module Data.Font (Font,newFont,Face,FaceIndex,newFileFace,RenderMode(..),RenderParams(..),defaultRenderParams,StringImage(..),renderString,asciiBitmap) where

import Definitive
import qualified Prelude as P
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import qualified Graphics.Rendering.FreeType.Internal as FT
import qualified Graphics.Rendering.FreeType.Internal.Library as FT
import qualified Graphics.Rendering.FreeType.Internal.FaceType as FT
import qualified Graphics.Rendering.FreeType.Internal.GlyphSlot as FT
import qualified Graphics.Rendering.FreeType.Internal.GlyphMetrics as FT
import qualified Graphics.Rendering.FreeType.Internal.PrimitiveTypes as FT
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as FTBMP
import qualified Graphics.Rendering.FreeType.Internal.Face  as FT hiding (height)
import qualified Graphics.Rendering.FreeType.Internal.Vector  as FT
import Control.Exception (bracket)

throwOnError :: IO FT.FT_Error -> IO ()
throwOnError act = act >>= \x -> if x /= 0 then error $ "FreeType returned error : "++show x else return ()

data Font = Font (ForeignPtr FT.FT_LibraryRec_) 
data Face = Face {
  _faceFont :: Font,
  _faceRepr :: ForeignPtr FT.FT_FaceRec_
  }

type Dynamic t = t -> IO (FunPtr t)
foreign import ccall "wrapper" mkFinalizer :: Dynamic (Ptr a -> IO ())

indirect :: Storable a => (Ptr a -> IO ()) -> IO a
indirect f = alloca $ \p -> f p >> peek p

newFont :: IO Font
newFont = do
  rawAddr <- indirect $ \p -> throwOnError $ FT.ft_Init_FreeType p
  ret <- newForeignPtr rawAddr (throwOnError $ FT.ft_Done_FreeType rawAddr)
  return (Font ret)

(.&) :: ((a -> IO c) -> IO c) -> ((b -> IO c) -> IO c) -> (a -> b -> IO c) -> IO c
ka .& kb = \kab -> ka $ \a -> kb $ \b -> kab a b

type FaceIndex = Int
newFileFace :: Font -> FilePath -> FaceIndex -> IO Face
newFileFace fnt path i = do
  rawFace <- indirect $ \pface -> do
    (withFontPtr fnt .& withCString path) $ \lib pathstr -> do
      throwOnError $ FT.ft_New_Face lib pathstr (fromIntegral i) pface
  ret <- newForeignPtr rawFace (throwOnError $ FT.ft_Done_Face rawFace)
  return (Face fnt ret)

data RenderMode = Grayscale | Monochromatic
data RenderParams = RenderParams {
  renderSize :: Int, -- ^ The maximum size (width or height) allowed for a single glyph in the rendition buffer, in pixels
  renderSizeAlignment :: Int, -- ^ The alignment required for the sizes of the returned buffer (OpenGL needs it to be a multiple of 4)
  renderMode :: RenderMode
  }
data CellMetrics = CellMetrics {
  cellLeftWidth, cellRightWidth,
  cellBottomHeight, cellTopHeight :: Int
  }
  deriving Show
instance Semigroup CellMetrics where
  CellMetrics lw rw bh th + CellMetrics lw' rw' bh' th' =
    CellMetrics (lw+rw) (lw'+rw') (max bh bh') (max th th')
data CellCoords = CellCoords {
  cellX,cellY :: Int,
  cellMetrics :: CellMetrics
  }
  deriving Show
data StringImage = StringImage {
  renditionWidth,renditionHeight :: Int,
  rendition :: Slice Word8,
  cells :: Map Char CellCoords
  }
  deriving Show

defaultRenderParams :: RenderParams
defaultRenderParams = RenderParams 72 4 Grayscale

renderString :: Face -> RenderParams -> String -> IO StringImage
renderString fc (RenderParams sz align mode) str = withFacePtr fc $ \fcp -> do
  slot <- peek (FT.glyph fcp)

  throwOnError $ FT.ft_Set_Pixel_Sizes fcp (fromIntegral sz) (fromIntegral sz)

  indices <- for str $ \c -> FT.ft_Get_Char_Index fcp (fromIntegral $ fromEnum c)
  metrics <- for indices $ \i -> do
    throwOnError $ FT.ft_Load_Glyph fcp i FT.ft_LOAD_NO_BITMAP
    -- putStrLn $ "Loading metrics for glyph "++show i
    peek (FT.metrics slot)
  let (sizeX,sizeY) = (foldMap (fromIntegral . FT.horiAdvance) (debug metrics) & toPixels, 
                       foldl1' max (map (fromIntegral . FT.height) metrics) & toPixels) :: (Int,Int)
      toPixels x = let y = ((x+63)`div`64)+align-1 in y-(y`mod`align)
      modeCode = case mode of
        Grayscale -> FT.ft_RENDER_MODE_NORMAL
        Monochromatic -> FT.ft_RENDER_MODE_MONO
  ret <- mallocForeignPtrArray (sizeX*sizeY)
  cs <- withForeignPtr ret $ \pret -> do
    -- putStrLn $ "Return buffer at "++show pret++" to "++show (pret`plusPtr`(sizeX*sizeY))
    pokeArray pret (take (sizeX*sizeY) $ repeat 0) 

    (\f -> foldr f (\_ -> return) (str `zip` indices `zip` metrics) 0 zero) $ \((c,i),m) k dx ret -> do
      throwOnError $ FT.ft_Load_Glyph fcp i FT.ft_LOAD_DEFAULT
      alloca $ \pv -> do poke pv (FT.FT_Vector 0 (FT.height m P.- FT.horiBearingY m))
                         FT.ft_Set_Transform fcp nullPtr pv
      throwOnError $ FT.ft_Render_Glyph slot modeCode
      bmp <- peek (FT.bitmap slot)
      let incr = fromIntegral (FTBMP.pitch bmp)
          h = fromIntegral (FTBMP.rows bmp)
          w = fromIntegral (FTBMP.width bmp)
          start | incr > 0 = FTBMP.buffer bmp`plusPtr`((h-1)*incr)
                | otherwise = FTBMP.buffer bmp
          rowPtrs = iterate (`plusPtr`negate incr) start
          adv = fromIntegral (FT.horiAdvance m`div`64)
      -- putStrLn $ "Copying rows of size "++show w++" from "++show start++" to "++show (pret`plusPtr`dx)++" (size "++show (sizeX-dx)++")"
      for_ (take h rowPtrs `zip` iterate (`plusPtr`sizeX) (pret `plusPtr` dx)) $ \(rowsrc,rowdst) -> do
        copyArray rowdst rowsrc w
  
      k (dx + adv) (insert c (CellCoords dx 0 (CellMetrics adv h
                                               (fromIntegral (FT.horiBearingX m)`div`64 + w`div`2)
                                               (fromIntegral (FT.height m P.- FT.horiBearingY m)`div`64))) ret)
  
  return (StringImage sizeX sizeY (V.unsafeFromForeignPtr0 ret (sizeX*sizeY)) cs)

deriving instance Show FTBMP.FT_Bitmap

asciiBitmap :: StringImage -> String
asciiBitmap (StringImage w h bmp _) = concat [[toChar (bmp V.! (y*w+x)) | x <- [0..w-1]]++"\n" | y <- [h-1,h-2..0]]
  where toChar x | x > 196 = '#'
                 | x > 127 = '+'
                 | x > 64 = '.'
                 | otherwise = ' '

withFontPtr :: Font -> (FT.FT_Library -> IO a) -> IO a
withFontPtr (Font p) = withForeignPtr p
withFacePtr :: Face -> (FT.FT_Face -> IO a) -> IO a
withFacePtr f = withForeignPtr (_faceRepr f)

ft_PIXEL_MODE_MONO = 1
ft_PIXEL_MODE_GRAY = 2
