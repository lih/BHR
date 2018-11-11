{-# LANGUAGE DeriveGeneric #-}
module Main where

import Definitive
import Algebra.Monad.Concatenative
import Control.Concurrent (threadDelay)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.StateVar as SV
import System.Environment (getArgs)
import Codec.Picture
import qualified Data.Vector.Storable as V
import Data.StateVar (($=))
import Foreign.Storable
import Foreign.Ptr
import Control.Exception (SomeException(..),Exception)
import GHC.Generics

instance (Storable a,Storable b) => Storable (a,b) where
  sizeOf x = sizeOf (fst x) + sizeOf (snd x)
  alignment x = lcm (alignment (fst x)) (alignment (snd x))
  peek p = do
    x <- peek (castPtr p)
    y <- peek (castPtr $ p`plusPtr`sizeOf x)
    return (x,y)
  poke p (x,y) = do
    poke (castPtr p) x
    poke (castPtr $ p`plusPtr`sizeOf x) y
instance (Storable a,Storable b,Storable c) => Storable (a,b,c) where
  sizeOf ~(x,y,z) = sizeOf (x,(y,z))
  alignment ~(x,y,z) = alignment (x,(y,z))
  peek p = peek (castPtr p) <&> \(x,(y,z)) -> (x,y,z)
  poke p (x,y,z) = poke (castPtr p) (x,(y,z))
instance (Storable a,Storable b,Storable c,Storable d) => Storable (a,b,c,d) where
  sizeOf ~(x,y,z,u) = sizeOf (x,(y,z,u))
  alignment ~(x,y,z,u) = alignment (x,(y,z,u))
  peek p = peek (castPtr p) <&> \(x,(y,z,u)) -> (x,y,z,u)
  poke p (x,y,z,u) = poke (castPtr p) (x,(y,z,u))
instance (Storable a,Storable b,Storable c,Storable d,Storable e) => Storable (a,b,c,d,e) where
  sizeOf ~(x,y,z,u,v) = sizeOf (x,(y,z,u,v))
  alignment ~(x,y,z,u,v) = alignment (x,(y,z,u,v))
  peek p = peek (castPtr p) <&> \(x,(y,z,u,v)) -> (x,y,z,u,v)
  poke p (x,y,z,u,v) = poke (castPtr p) (x,(y,z,u,v))

stringWords :: String -> [String]
stringWords = map fromString . fromBlank
  where fromBlank (c:t) | c `elem` [' ', '\t', '\r', '\n'] = fromBlank t
                        | c == '"' = fromQuote id t
                        | otherwise = fromWChar (c:) t
        fromBlank "" = []
        fromQuote k ('"':t) = ('"':k "\""):fromBlank t
        fromQuote k ('\\':c:t) = fromQuote (k.(qChar c:)) t
          where qChar 'n' = '\n' ; qChar 't' = '\t' ; qChar x = x
        fromQuote k (c:t) = fromQuote (k.(c:)) t
        fromQuote k "" = ['"':k "\""]
        fromWChar k (c:t) | c `elem` [' ', '\t', '\r', '\n'] = k "":fromBlank t
                          | otherwise = fromWChar (k.(c:)) t
        fromWChar k "" = [k ""]

data LogosBuiltin = Wait | Quit | Format | Print | OpenWindow | Point | Color Bool | Texture | TextureCoord | Draw | BindTexture
                  deriving Show
data LogosData = P (GL.Vertex3 GL.GLfloat) | C (GL.Color4 GL.GLfloat) | T (GL.TexCoord2 GL.GLfloat) | TI GL.TextureObject
               deriving Show
data LogosState = LogosState {
  _running :: Bool
  }
running :: Lens' LogosState Bool
running = lens _running (\x y -> x { _running = y })

dict = fromAList $ map (second StackBuiltin) $
  [("wait"       , Builtin_Extra Wait  ),
   ("quit"       , Builtin_Extra Quit  ),
   ("format"     , Builtin_Extra Format),
   ("print"      , Builtin_Extra Print ),
   ("window"     , Builtin_Extra OpenWindow),
   ("point"      , Builtin_Extra Point),
   ("rgb"        , Builtin_Extra (Color False)),
   ("rgba"       , Builtin_Extra (Color True)),
   ("texture"    , Builtin_Extra Texture),
   ("texbind"    , Builtin_Extra BindTexture),
   ("texpoint"   , Builtin_Extra TextureCoord),
   ("draw"       , Builtin_Extra Draw),
                   
   ("def"        , Builtin_Def         ),
   ("$"          , Builtin_DeRef       ),
   ("lookup"     , Builtin_Lookup      ),
   ("exec"       , Builtin_Exec        ),
   ("quote"      , Builtin_Quote       ),
   
   ("stack"      , Builtin_Stack       ),
   ("clear"      , Builtin_Clear       ),
   ("shift"      , Builtin_Shift       ),
   ("shaft"      , Builtin_Shaft       ),
   ("pop"        , Builtin_Pop         ),
   ("popn"       , Builtin_PopN        ),
   ("dup"        , Builtin_Dup         ),
   ("dupn"       , Builtin_DupN        ),
   ("swap"       , Builtin_Swap        ),
   ("swapn"      , Builtin_SwapN       ),
   ("pick"       , Builtin_Pick        ),
   
   ("["          , Builtin_ListBegin   ),
   ("]"          , Builtin_ListEnd     ),
   
   ("+"          , Builtin_Add         ),
   ("-"          , Builtin_Sub         ),
   ("*"          , Builtin_Mul         ),
   ("div"        , Builtin_Div         ),
   ("mod"        , Builtin_Mod         ),
   ("sign"       , Builtin_Sign        ),
   
   ("each"       , Builtin_Each        ),
   ("range"      , Builtin_Range       ),
   
   ("vocabulary" , Builtin_CurrentDict ),
   ("empty"      , Builtin_Empty       ),
   ("insert"     , Builtin_Insert      ),
   ("delete"     , Builtin_Delete      ),
   ("keys"       , Builtin_Keys        )]

fromStack (StackSymbol x) = read x :: GL.GLfloat
fromStack (StackInt n) = fromIntegral n
fromStack _ = undefined

runLogos Wait = do
  st <- runStackState get
  case st of
    StackInt n:st' -> do
      liftIO $ threadDelay n
      runStackState $ put st'
    _ -> unit
runLogos Quit = runExtraState $ do running =- False
runLogos Format = do
  st <- runStackState get
  case st of
    StackSymbol str:st' -> do
      let format ('%':'s':xs) (h:t) = second (showV h+) $ format xs t
          format (x:xs) l = second (x:) $ format xs l
          format _ st' = (st',"")
          showV (StackExtra (Opaque x)) = show x
          showV (StackList l) = "["+intercalate "," (map showV l)+"]"
          showV x = show x
          (st'',msg) = format str st'
      runStackState $ put (StackSymbol msg:st'')
    _ -> unit
runLogos Print = do
  st <- runStackState get
  case st of
    StackSymbol str:st' -> liftIO (putStr str) >> runStackState (put st')
    _ -> unit
runLogos OpenWindow = do
  st <- runStackState get
  case st of
    StackInt h:StackInt w:st' -> do
      runStackState $ put st'
      liftIO $ do
        GLFW.openWindowHint GLFW.FSAASamples 4
        GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
        GLFW.openWindowHint GLFW.OpenGLVersionMinor 3
        GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile
 
        success <- GLFW.openWindow (GL.Size (fromIntegral w) (fromIntegral h)) [GLFW.DisplayRGBBits 8 8 8, GLFW.DisplayAlphaBits 8, GLFW.DisplayDepthBits 8] GLFW.Window
        if not success then putStrLn "Failed to open OpenGL window" else (initGL >> initShaders)
    _ -> unit
runLogos Point = do
  st <- runStackState get
  case st of
    (fromStack -> z):(fromStack -> y):(fromStack -> x):st' -> do
      runStackState $ put $ StackExtra (Opaque (P (GL.Vertex3 x y z))):st'
    _ -> unit
runLogos (Color isRGBA) = do
  st <- runStackState get
  case st of
    (fromStack -> a):(fromStack -> b):(fromStack -> g):(fromStack -> r):st' | isRGBA -> do
      runStackState $ put $ StackExtra (Opaque (C (GL.Color4 r g b a))):st'
    (fromStack -> b):(fromStack -> g):(fromStack -> r):st' | not isRGBA -> do
      runStackState $ put $ StackExtra (Opaque (C (GL.Color4 r g b 1.0))):st'
    _ -> unit
runLogos TextureCoord = do
  st <- runStackState get
  case st of
    (fromStack -> y):(fromStack -> x):st' -> do
      runStackState $ put $ StackExtra (Opaque (T (GL.TexCoord2 x y))):st'
    _ -> unit
runLogos BindTexture = do
  st <- runStackState get
  case st of
    StackExtra (Opaque (TI tex)):st' -> do
      liftIO $ do
        GL.textureBinding GL.Texture2D $= Just tex
      runStackState $ put st'
    _ -> unit
runLogos Texture = do
  st <- runStackState get
  case st of
    StackSymbol file:st' -> do
      runStackState (put st')
      textureLoaded <- liftIO $ do
        imgbytes <- readChunk file
        let img = convertRGB8 <$> decodeImage imgbytes
        tex <- GL.genObjectName
        case img of
          Right (Image w h imgd) -> do
            GL.textureBinding GL.Texture2D $= Just tex
            V.unsafeWith imgd $ \imgp -> do
              GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (GL.PixelData GL.BGR GL.UnsignedByte imgp)
            GL.textureFilter GL.Texture2D $= ((GL.Linear',Nothing),GL.Linear')
            GL.generateMipmap' GL.Texture2D
            GL.textureBinding GL.Texture2D $= Nothing
            return $ Just tex
          Left err -> do
            putStrLn err
            return Nothing
      case textureLoaded of
        Just tex -> runStackState $ modify (StackExtra (Opaque (TI tex)):)
        Nothing -> unit

    _ -> unit

runLogos Draw = do
  st <- runStackState get
  case st of
    StackSymbol s:StackList l:st' -> do
      runStackState $ put st'
      liftIO $ do
        let mode = case s of
              "lines" -> GL.Lines
              "triangles" -> GL.Triangles
              "points" -> GL.Points
              _ -> GL.Points
            extras = [x | StackExtra (Opaque x) <- l]
            fullVertices = go zacc extras
              where zacc = (GL.Color4 0 0 0 0,GL.TexCoord2 0 0)
                    go (c,tx) (P v:t) = (c,tx,v):go zacc t
                    go (_,tx) (C c:t) = go (c,tx) t
                    go (c,_)  (T tx:t) = go (c,tx) t
                    go acc      (h:t) = go acc t
                    go _ [] = []
            newVec f = GL.genObjectName <*= \vb -> do
              let vs = V.unfoldr (\case
                                     h:t -> Just (f h,t)
                                     [] -> Nothing) fullVertices
              GL.bindBuffer GL.ArrayBuffer $= Just vb
              V.unsafeWith vs $ \p -> do
                GL.bufferData GL.ArrayBuffer $= (fromIntegral (V.length vs * sizeOf (vs V.! 0)),p,GL.StaticDraw)
        
        cb <- newVec (\(h,_,_) -> h)
        tb <- newVec (\(_,h,_) -> h)
        vb <- newVec (\(_,_,h) -> h)

        GL.clear [ GL.DepthBuffer, GL.ColorBuffer ]

        let withAttrib n = between (GL.vertexAttribArray (GL.AttribLocation n) $= GL.Enabled) (GL.vertexAttribArray (GL.AttribLocation n) $= GL.Disabled)
        for_ [i | TI i <- extras] $ \(GL.TextureObject i) -> do
          GL.uniform (GL.UniformLocation 0) $= GL.TextureUnit (debug i)
        withAttrib 0 $ withAttrib 1 $ withAttrib 2 $ do
          GL.bindBuffer GL.ArrayBuffer $= Just vb
          GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
          GL.bindBuffer GL.ArrayBuffer $= Just cb
          GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 0 nullPtr)
          GL.bindBuffer GL.ArrayBuffer $= Just tb
          GL.vertexAttribPointer (GL.AttribLocation 2) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)
          GL.drawArrays mode 0 (fromIntegral $ length fullVertices)
        GLFW.swapBuffers
    _ -> unit

data GLSLCompileException = GLSLShaderCompileError String | GLSLProgramLinkError String
  deriving (Show,Generic)
instance Exception GLSLCompileException

initShaders = GL.createProgram >>= \prog -> do
  let compileShader shType shFile = GL.createShader shType <*= \vs -> do
        body <- readChunk shFile
        GL.shaderSourceBS vs $= body
        GL.compileShader vs
        success <- SV.get (GL.compileStatus vs)
        if success then
          GL.attachShader prog vs
          else throw . SomeException . GLSLShaderCompileError =<< SV.get (GL.shaderInfoLog vs)
  compileShader GL.VertexShader "vertex.shader"
  compileShader GL.FragmentShader "fragment.shader"
  
  GL.linkProgram prog
  success <- SV.get (GL.linkStatus prog)
  if success then 
    GL.currentProgram $= Just prog
    else
    throw . SomeException . GLSLProgramLinkError =<< SV.get (GL.programInfoLog prog)

initGL = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  
  GL.depthFunc            $= Just GL.Lequal
  GL.blend                $= GL.Enabled
  GL.blendFunc            $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureFunction      $= GL.Blend

main = do
  putStrLn "Initializing graphical environment..."
  between (void GLFW.initialize) GLFW.terminate $ do
    args <- getArgs
    prelude <- fold <$> for args readString
    text <- readHString stdin
    let go (w:ws) = do
          execSymbol runLogos (\_ -> unit) w
          r <- runExtraState $ getl running
          if r then go ws else unit
        go [] = unit
    (go (stringWords (prelude + " " + text))^..stateT.concatT) (defaultState dict (LogosState True))
        
