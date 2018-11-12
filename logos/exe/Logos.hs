{-# LANGUAGE DeriveGeneric, TypeFamilies, ScopedTypeVariables #-}
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

data Zero = Zero
data Succ x = Succ x

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three

_One = Succ Zero
_Two = Succ _One
_Three = Succ _Two
_Four = Succ _Three

data family Vec n :: * -> *
data instance Vec Zero a = V0
data instance Vec (Succ n) a = VS a (Vec n a) 

instance Functor (Vec Zero) where
  map f V0 = V0
instance Functor (Vec n) => Functor (Vec (Succ n)) where
  map f (VS x xs) = VS (f x) (map f xs)
instance SemiApplicative (Vec Zero) where
  V0 <*> V0 = V0
instance SemiApplicative (Vec n) => SemiApplicative (Vec (Succ n)) where
  VS f fs <*> VS x xs = VS (f x) (fs<*>xs)
instance Unit (Vec Zero) where pure _ = V0
instance Unit (Vec n) => Unit (Vec (Succ n)) where pure x = VS x (pure x)
instance Applicative (Vec Zero)
instance Applicative (Vec n) => Applicative (Vec (Succ n))
instance Foldable (Vec Zero) where fold V0 = zero
instance Foldable (Vec n) => Foldable (Vec (Succ n)) where fold (VS a x) = a + fold x
instance Traversable (Vec Zero) where sequence V0 = pure V0
instance Traversable (Vec n) => Traversable (Vec (Succ n)) where sequence (VS a x) = VS<$>a<*>sequence x
class (Applicative v, Foldable v, Traversable v) => Vector v
instance Vector (Vec Zero)
instance Vector (Vec n) => Vector (Vec (Succ n))

type V1 = Vec One
type V2 = Vec Two
type V3 = Vec Three
type V4 = Vec Four

instance (Semigroup a,Applicative (Vec n)) => Semigroup (Vec n a) where a + b = liftA2 (+) a b
instance (Monoid a,Applicative (Vec n)) => Monoid (Vec n a) where zero = pure zero

type Mat n m a = Vec n (Vec m a)
toMat :: Vec n a -> Mat One n a
toMat = pure
matMult :: (Ring a, Vector (Vec n), Vector (Vec m), Vector (Vec p)) => Mat n m a -> Mat m p a -> Mat n p a
matMult x y = map (\vm -> map (\vm' -> scalProd vm vm') (transpose y)) x

scalProd :: (Ring a,Vector (Vec n)) => Vec n a -> Vec n a -> a
scalProd u v = sum (liftA2 (*) u v)
  
data LogosBuiltin = Wait | Quit | Format | Print | OpenWindow | Point | Color Bool | Texture | TextureCoord | Draw | BindTexture
                  deriving Show
-- data VertexInfo = VertexInfo !(GL.Vector3 GL.GLfloat) !(GL.Color4 GL.GLfloat) !(GL.TexCoord2 GL.GLfloat)
-- data Mesh = Mesh GL.PrimitiveMode [VertexInfo]
-- data Scene = OriginMesh Mesh | Subscenes [TransformedScene]
-- type TransformedScene = ([Transform],Scene)

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
      void $ liftIO $ do
        GLFW.openWindowHint GLFW.FSAASamples 4
        GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
        GLFW.openWindowHint GLFW.OpenGLVersionMinor 3
        GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile
 
        success <- GLFW.openWindow (GL.Size (fromIntegral w) (fromIntegral h)) [GLFW.DisplayRGBBits 8 8 8, GLFW.DisplayAlphaBits 8, GLFW.DisplayDepthBits 8] GLFW.Window
        if not success then throw $ SomeException GLFWWindowOpenException else (initGL >> initShaders)
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
        
        Just prog <- SV.get GL.currentProgram
        m <- GL.newMatrix GL.ColumnMajor [1,0,0,0 , 0,1,0,0 , 0,0,1,0 , 0,0,0,1]
        vpu <- GL.uniformLocation prog "viewMat"
        GL.uniform vpu $= (m :: GL.GLmatrix GL.GLfloat)
        SV.get (GL.activeUniforms prog) >>= print

        cb <- newVec (\(h,_,_) -> h)
        tb <- newVec (\(_,h,_) -> h)
        vb <- newVec (\(_,_,h) -> h)

        let withAttrib n f = do
              l <- SV.get (GL.attribLocation prog n)
              between (GL.vertexAttribArray l $= GL.Enabled) (GL.vertexAttribArray l $= GL.Disabled) (f l)
            setAttrib b v n = do
              GL.bindBuffer GL.ArrayBuffer $= Just b
              GL.vertexAttribPointer v $= (GL.ToFloat, GL.VertexArrayDescriptor n GL.Float 0 nullPtr)

        GL.clear [ GL.DepthBuffer, GL.ColorBuffer ]

        withAttrib "vertexPosition" $ \vpos -> withAttrib "vertexColor" $ \vcol -> withAttrib "vertexUV" $ \vtex -> do
          setAttrib vb vpos 3
          setAttrib cb vcol 4
          setAttrib tb vtex 2
          GL.drawArrays mode 0 (fromIntegral $ length fullVertices)
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

