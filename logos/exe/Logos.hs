{-# LANGUAGE DeriveGeneric, TypeFamilies, ScopedTypeVariables, ExistentialQuantification, PatternSynonyms #-}
module Main where

import Algebra.Monad.Concatenative
import Codec.Picture hiding (Uniform)
import Console.Readline (readline,addHistory,setCompletionEntryFunction)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException(..),Exception)
import Data.IORef
import Data.Matricial
import Data.StateVar (($=))
import Definitive
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import Language.Parser
import System.Environment (getArgs)
import System.IO (hIsTerminalDevice)
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.StateVar as SV
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

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
  
data LogosBuiltin = Wait | Quit | Format | Print | OpenWindow | Texture | BuildMesh | Draw | Uniform | DefUniform
                  | VCons | MCons | Rotation | Translation | Skew | Ejection | MCompose | MAdd
                  deriving Show
toFloat (StackInt n) = Just (fromIntegral n)
toFloat (StackSymbol s) = matches Just readable s
toFloat (StackExtra (Opaque (F f))) = Just f
toFloat x = Nothing

pattern StackFloat f <- (toFloat -> Just f)
pattern StackVect v = StackExtra (Opaque (V v))
pattern StackMat m = StackExtra (Opaque (M m))

data LogosData = F GL.GLfloat
               | V (V4 GL.GLfloat)
               | M (Mat Four Four GL.GLfloat)
               | Mesh GL.PrimitiveMode Int [(String,Int,GL.BufferObject)]
               | Uni GL.UniformLocation
               | TI GL.TextureObject
               deriving Show
data LogosState = LogosState {
  _running :: Bool
  }
running :: Lens' LogosState Bool
running = lens _running (\x y -> x { _running = y })

dict = fromAList $
  map (second StackBuiltin)
  [("wait"        , Builtin_Extra Wait  ),
   ("quit"        , Builtin_Extra Quit  ),
   ("format"      , Builtin_Extra Format),
   ("vcons"       , Builtin_Extra VCons),
   ("mcons"       , Builtin_Extra MCons),
   ("rotation"    , Builtin_Extra Rotation),
   ("translation" , Builtin_Extra Translation),
   ("**"          , Builtin_Extra MCompose),
   ("++"          , Builtin_Extra MAdd),
   ("skew"        , Builtin_Extra Skew),
   ("ejection"    , Builtin_Extra Ejection),
   ("print"       , Builtin_Extra Print),
   ("window"      , Builtin_Extra OpenWindow),
   ("texture"     , Builtin_Extra Texture),
   ("mesh"        , Builtin_Extra BuildMesh),
   ("draw"        , Builtin_Extra Draw),
   ("uniform"     , Builtin_Extra Uniform),
   ("defuniform"     , Builtin_Extra DefUniform),
    
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
runLogos VCons = runStackState $ modify $ \case
  StackFloat w:StackFloat z:StackFloat y:StackFloat x:st -> StackVect (V4 x y z w):st
  st -> st
runLogos MCons = runStackState $ modify $ \case
  StackVect w:StackVect z:StackVect y:StackVect x:st -> StackMat (V4 x y z w):st
  st -> st
runLogos Rotation = runStackState $ modify $ \case
  StackVect u:StackVect v:st -> StackMat (rotation v u):st
  st -> st
runLogos Translation = runStackState $ modify $ \case
  StackVect (V4 x y z _):st -> StackMat (translation (V3 x y z)):st
  st -> st
runLogos Ejection = runStackState $ modify $ \case
  StackVect v:st -> StackMat (ejection v):st
  st -> st
runLogos Skew = runStackState $ modify $ \case
  StackVect v:st -> StackMat (skew v):st
  st -> st
runLogos MAdd = runStackState $ modify $ \case
  StackMat m:StackMat m':st -> StackMat (m+m'):st
  StackVect v:StackVect v':st -> StackVect (v+v'):st
  StackFloat f:StackFloat f':st -> StackExtra (Opaque $ F $ f+f'):st
  st -> st
runLogos MCompose = runStackState $ modify $ \case
  StackMat m':StackMat m:st -> StackMat (m'$*m):st
  StackMat m:StackVect v:st -> StackVect (v & from scalar %~ ($*m)):st
  StackVect v:StackMat m:st -> StackVect (v & from scalar %~ ($*m)):st
  StackVect v:StackVect v':st -> StackExtra (Opaque $ F $ scalProd v v'):st
  StackFloat f:StackVect v:st -> StackVect (pure f * v):st
  StackVect v:StackFloat f:st -> StackVect (pure f * v):st
  StackFloat f:StackMat m:st -> StackMat (map2 (f*) m):st
  StackMat m:StackFloat f:st -> StackMat (map2 (f*) m):st
  st -> st
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
      void $ liftIO $ do
        GLFW.openWindowHint GLFW.FSAASamples 4
        GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
        GLFW.openWindowHint GLFW.OpenGLVersionMinor 3
        GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile
 
        success <- GLFW.openWindow (GL.Size (fromIntegral w) (fromIntegral h)) [GLFW.DisplayRGBBits 8 8 8, GLFW.DisplayAlphaBits 8, GLFW.DisplayDepthBits 8] GLFW.Window
        if not success then throw $ SomeException GLFWWindowOpenException else (initGL >> initShaders)
    _ -> unit
runLogos Uniform = do
  st <- runStackState get
  case st of
    StackSymbol name:st' -> do
      i <- liftIO $ do
        Just p <- SV.get GL.currentProgram
        SV.get (GL.uniformLocation p name)
      runStackState $ put (StackExtra (Opaque (Uni i)):st')
    _ -> unit
runLogos DefUniform = do
  st <- runStackState get
  case st of
    x:StackExtra (Opaque (Uni u)):st' -> do
      runStackState $ put st'
      case x of
        StackVect (V4 x y z w) -> liftIO $ GL.uniform u $= GL.Vector4 x y z w
        StackMat (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) -> liftIO $ do
          m <- GL.newMatrix GL.RowMajor [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]
          GL.uniform u $= (m :: GL.GLmatrix GL.GLfloat)
        _ -> unit
    _ -> unit
      
runLogos Texture = do
  st <- runStackState get
  case st of
    StackSymbol file:StackSymbol name:st' -> do
      runStackState (put st')
      textureLoaded <- liftIO $ do
        imgbytes <- readChunk file
        let img = convertRGB8 <$> decodeImage imgbytes
        tex@(GL.TextureObject texi) <- GL.genObjectName
        case img of
          Right (Image w h imgd) -> do
            GL.activeTexture $= GL.TextureUnit texi
            GL.textureBinding GL.Texture2D $= Just tex
            V.unsafeWith imgd $ \imgp -> do
              GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (GL.PixelData GL.RGB GL.UnsignedByte imgp)
            GL.textureFilter GL.Texture2D $= ((GL.Linear',Nothing),GL.Linear')
            GL.generateMipmap' GL.Texture2D
            Just prog <- SV.get GL.currentProgram
            ul <- GL.uniformLocation prog name
            GL.uniform (debug ul) $= GL.TextureUnit texi
            return $ Just tex
          Left err -> do
            putStrLn err
            return Nothing
      case textureLoaded of
        Just tex -> runStackState $ modify (StackExtra (Opaque (TI tex)):)
        Nothing -> unit

    _ -> unit

runLogos BuildMesh = do
  st <- runStackState get
  case st of
    StackSymbol s:StackList attribs:StackList props:st' -> do
      m <- liftIO $ do
        let mode = case s of
              "LINES" -> GL.Lines
              "TRIANGLES" -> GL.Triangles
              "POINTS" -> GL.Points
              _ -> GL.Points
            fullVertices = deZip $ traverse Zip [[v | StackVect v <- vs] | StackList vs <- props]
            newVec f l = GL.genObjectName <*= \vb -> do
              let vs = V.unfoldr (\case
                                     h:t -> Just (f h,t)
                                     [] -> Nothing) l
              GL.bindBuffer GL.ArrayBuffer $= Just vb
              V.unsafeWith vs $ \p -> do
                GL.bufferData GL.ArrayBuffer $= (fromIntegral (V.length vs * sizeOf (vs V.! 0)),p,GL.StaticDraw)

        vecs <- sequence (zap [let run = case n of
                                     1 -> newVec (\(V4 x _ _ _) -> V1 x)
                                     2 -> newVec (\(V4 x y _ _) -> V2 x y)
                                     3 -> newVec (\(V4 x y z _) -> V3 x y z)
                                     4 -> newVec id
                                     _ -> error $ "Invalid attribute size "+show n+" (must be between 1 and 4)"
                               in \l -> run l <&> (s,n,)
                              | StackList [StackSymbol s,StackInt n] <- attribs] fullVertices)
        return (Mesh mode (length (head fullVertices)) vecs)
      runStackState $ put (StackExtra (Opaque m):st')
    _ -> unit
      
runLogos Draw = do
  st <- runStackState get
  case st of
    StackExtra (Opaque (Mesh mode size vecs)):st' -> do
      runStackState $ put st'
      liftIO $ do
        Just prog <- SV.get GL.currentProgram
        m <- GL.newMatrix GL.ColumnMajor [1,0,0,0 , 0,1,0,0 , 0,0,1,0 , 0,0,0,1]
        vpu <- GL.uniformLocation prog "viewMat"
        GL.uniform vpu $= (m :: GL.GLmatrix GL.GLfloat)
        SV.get (GL.activeUniforms prog) >>= print

        let withAttrib (name,sz,vec) go = do
              l <- SV.get (GL.attribLocation prog name)
              between (GL.vertexAttribArray l $= GL.Enabled) (GL.vertexAttribArray l $= GL.Disabled) $ do
                GL.bindBuffer GL.ArrayBuffer $= Just vec
                GL.vertexAttribPointer l $= (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral sz) GL.Float 0 nullPtr)
                go

        GL.clear [ GL.DepthBuffer, GL.ColorBuffer ]
        
        composing withAttrib vecs $ do
          GL.drawArrays mode 0 (fromIntegral size)

        GLFW.swapBuffers
    _ -> unit

data GLSLCompileException = GLSLShaderCompileError String | GLSLProgramLinkError String
  deriving (Show,Generic)
instance Exception GLSLCompileException
data GLFWException = GLFWWindowOpenException
  deriving (Show,Generic)
instance Exception GLFWException

initShaders = GL.createProgram <*= \prog -> do
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

main = between (void GLFW.initialize) GLFW.terminate $ do
  isTerm <- hIsTerminalDevice stdin
  args <- getArgs
  prelude <- fold <$> for args readString
  symList <- newIORef (keys (c'map dict))
  let getAll = unsafeInterleaveIO $ do
        ln <- readline "Logos> " 
        lns <- getAll
        case ln of
          Just x -> do addHistory x; return $ x + " .\n" + lns
          Nothing -> putStr "\n" >> return ""
  setCompletionEntryFunction $ Just $ \line -> do
    sl <- readIORef symList
    case reverse (words (line+"?")) of
      "?":_ -> return sl
      wp:_ -> let wps = length wp-1; wp' = init wp in return [w | w <- sl, take wps w==wp']
      _ -> return []
  text <- if isTerm then getAll else readHString stdin

  let go (w:ws) = do
        execSymbol runLogos (\_ -> unit) w
        runDictState get >>= \d -> liftIO (writeIORef symList (keys d))
        r <- runExtraState $ getl running
        if r then go ws else unit
      go [] = unit
  (go (stringWords (prelude + " " + text))^..stateT.concatT) (defaultState dict (LogosState True))

instance Storable (Vec Zero a) where
  sizeOf _ = zero
  alignment _ = 1
  peek p = return V0
  poke _ _ = unit
instance (Storable a,Storable (Vec n a)) => Storable (Vec (Succ n) a) where
  sizeOf ~(VS x v) = sizeOf x + sizeOf v
  alignment ~(VS x v) = alignment x
  peek p = peek (castPtr p) <&> uncurry VS
  poke p (VS x v) = poke (castPtr p) (x,v)

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

