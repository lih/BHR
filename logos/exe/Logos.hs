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
data LogosData = P (GL.Vertex3 GL.GLdouble) | C (GL.Color4 GL.GLdouble) | T (GL.TexCoord2 GL.GLdouble) | TI GL.TextureObject
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

fromStack (StackSymbol x) = read x :: GL.GLdouble
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
        void $ GLFW.openWindow (GL.Size (fromIntegral w) (fromIntegral h)) [GLFW.DisplayRGBBits 8 8 8, GLFW.DisplayAlphaBits 8, GLFW.DisplayDepthBits 8] GLFW.Window
        
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
        putStrLn $ "Binding texture "+show tex
        GL.textureBinding GL.Texture2D SV.$= Just tex
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
            GL.textureBinding GL.Texture2D SV.$= Just tex
            V.unsafeWith imgd $ \imgp -> do
              GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (GL.PixelData GL.BGR GL.UnsignedByte imgp)
            GL.textureFilter GL.Texture2D SV.$= ((GL.Linear',Nothing),GL.Linear')
            GL.generateMipmap' GL.Texture2D
            GL.textureBinding GL.Texture2D SV.$= Nothing
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
        GL.clear [ GL.DepthBuffer, GL.ColorBuffer ]
        let mode = case s of
              "lines" -> GL.Lines
              "triangles" -> GL.Triangles
              "points" -> GL.Points
              _ -> GL.Points
        GL.renderPrimitive mode $ for_ l $ \case
          StackExtra (Opaque (P v)) -> GL.vertex v
          StackExtra (Opaque (C c)) -> GL.color c
          StackExtra (Opaque (T t)) -> GL.texCoord t
          _ -> unit
        GLFW.swapBuffers
    _ -> unit

main = do
  putStrLn "Initializing graphical environment..."
  between (void GLFW.initialize) GLFW.terminate $ do
    args <- getArgs
  
    GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCompatProfile
    GL.depthFunc            SV.$= Just GL.Lequal
    GL.blend                SV.$= GL.Enabled
    GL.blendFunc            SV.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.texture GL.Texture2D SV.$= GL.Enabled
    GL.textureFunction      SV.$= GL.Blend

    prelude <- fold <$> for args readString
    putStrLn "Hello from Logos !"
    text <- readHString stdin
    let go (w:ws) = do
          execSymbol runLogos (\_ -> unit) w
          r <- runExtraState $ getl running
          if r then go ws else unit
        go [] = unit
    (go (stringWords (prelude + " " + text))^..stateT.concatT) (defaultState dict (LogosState True))
        
