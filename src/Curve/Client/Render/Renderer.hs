{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Client.Render.Renderer where

import           Control.Concurrent
import           Control.Applicative
{-import           Data.Foldable-}
import           Data.List
import           Data.Vec hiding (map, get)

import           Foreign.Storable (sizeOf)
import           Foreign.Ptr
import           Graphics.GLUtil.Camera3D (deg2rad)

import qualified Graphics.UI.GLUT as GLUT
import           Graphics.Rendering.OpenGL hiding (perspective)
import           Graphics.Rendering.OpenGL.Raw
import           Graphics.GLUtil
import           Graphics.GLUtil.VertexArrayObjects

{-import qualified Data.Vec as Vec-}

import           Curve.Client.Types


{-data Shaders = Shaders { prog              :: Program-}
                        {-,uColor            :: UniformLocation-}
                        {-,uModelMatrix      :: UniformLocation-}
                        {-,uViewMatrix       :: UniformLocation-}
                        {-,uProjectionMatrix :: UniformLocation-}
                        {-,vPosition         :: AttribLocation }-}

{-data Resources = Resources { vertexBuffer  :: BufferObject-}
                            {-,elementBuffer :: BufferObject-}
                            {-,shaders       :: Shaders-}
                            {-,fadeFactor    :: GLfloat }-}


vertexBufferData :: [GLfloat]
vertexBufferData = [-1, -1, 1, -1, -1, 1, 1, 1]

cube :: GLfloat -> [GLfloat]
cube w = (concatMap (\(a,b,c) -> [a,b,c]) )
      [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
        ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
        ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
        (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
        ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
        ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]




{-initShaders = do -}
  {-vs :: VertexShader   <- loadShader "src/Curve/Client/Render/Shader/HelloWorld.vs"-}
  {-fs :: FragmentShader <- loadShader "src/Curve/Client/Render/Shader/HelloWorld.fs"-}
  {-p <- linkShaderProgram [vs] [fs]-}
  {-Shaders p-}
    {-<$> get (uniformLocation p "uColor")-}
    {-<*> get (uniformLocation p "uModelMatrix")-}
    {-<*> get (uniformLocation p "uViewMatrix")-}
    {-<*> get (uniformLocation p "uProjectionMatrix")-}
    {-<*> get (attribLocation  p "vPosition")-}

{-makeResources =  Resources-}
             {-<$> makeBuffer ArrayBuffer vertexBufferData-}
             {-<*> makeBuffer ElementArrayBuffer elementBufferData-}
             {-<*> initShaders-}
             {-<*> pure 0.0   -}

{-setupGeometry :: Resources -> IO ()-}
{-setupGeometry r = let posn = vPosition (shaders r)-}
                      {-stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2-}
                      {-vad = VertexArrayDescriptor 2 Float stride offset0-}
                  {-in do bindBuffer ArrayBuffer   $= Just (vertexBuffer r)-}
                        {-vertexAttribPointer posn $= (ToFloat, vad)-}
                        {-vertexAttribArray posn   $= Enabled-}


{-fooVAO :: GLfloat -> Shaders  -> IO (VertexArrayObject)-}
{-fooVAO w s = do-}
  {-ab  <- makeBuffer ArrayBuffer (cube w)-}

  {-let vPos = vPosition s-}
  {-let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 3-}
  {-let vad = VertexArrayDescriptor 3 Float stride offset0-}

  {-makeVAO $ do-}
    {-bindBuffer ArrayBuffer   $= Just ab-}
    {-vertexAttribPointer vPos $= (ToFloat, vad)-}
    {-vertexAttribArray vPos   $= Enabled-}




draw :: IO ()
draw = do
  {-s <- initShaders-}
  {-clearColor $= Color4 0 0 0 1-}
  {-clear [ColorBuffer]-}
  {-currentProgram $= Just (prog s)-}

  {---set color-}
  {-uniformVec (uColor s)      $= [1,0,1]-}
  {-uniformMat (uViewMatrix s) $= [[  1,   0,   0,   0],-}
                                 {-[  0,   1,   0,   0],-}
                                 {-[  0,   0,   1,   0],-}
                                 {-[  0,   0,   0,   1]]-}


  {-let trans = (translation (fromList [0, 0, -10]) :: Mat44 GLfloat)-}
  {-let rot   = (rotationX 1) `multmm` (rotationY 1)-}
  {-uniformMat (uModelMatrix      s) $= (matToLists) (trans `multmm` rot)-}
  {-uniformMat (uProjectionMatrix s) $= (matToLists) (perspective 0.01 100 (deg2rad 30) 1 :: Mat44 GLfloat)-}

  {-vao2 <- fooVAO 1 s-}
  {-vao3 <- fooVAO 2 s-}
  {-withVAO vao3 $ do drawArrays Quads 0 (4*6)-}


  s <- initBasicShader
  clearColor $= Color4 0 0.1 0.1 1
  clear [ColorBuffer, DepthBuffer]
  depthMask $= Enabled
  depthFunc $= Just Lequal
  currentProgram $= Just (basic_program s)

  uniformVec (basic_uColor s)      $= [1,0,1]
  uniformMat (basic_uViewMatrix s) $= [[  1,   0,   0,   0],
                                       [  0,   1,   0,   0],
                                       [  0,   0,   1,   0],
                                       [  0,   0,   0,   1]]


  let trans = (translation (fromList [0, 0, -10]) :: Mat44 GLfloat)
  let rot   = (rotationX 1) `multmm` (rotationY 1)
  uniformMat (basic_uModelMatrix      s) $= (matToLists) (trans `multmm` rot)
  uniformMat (basic_uProjectionMatrix s) $= (matToLists) (perspective 0.01 100 (deg2rad 30) 1 :: Mat44 GLfloat)

  vao1 <- makePaddleVAO s
  withVAO vao1 $ do drawArrays Quads 0 (4*6)
  GLUT.swapBuffers



display :: MVar Env -> IO ()
display envar = do
  {-clear [ ColorBuffer ]-}
  {-flush-}
  draw

-----------------------------------------------------------
-- TODO shader programming:
{-http://www.arcadianvisions.com/blog/?p=224-}

{-makeTexture :: FilePath -> IO TextureObject -}
{-makeTexture filename = -}
    {-[>do (width,height,pixels) <- readTGA filename <]-}
       {-texture <- loadTexture $ texInfo width height TexBGR pixels-}
       {-textureFilter   Texture2D   $= ((Linear', Nothing), Linear') -}
       {-textureWrapMode Texture2D S $= (Mirrored, ClampToEdge) -}
       {-textureWrapMode Texture2D T $= (Mirrored, ClampToEdge) -}
       {-return texture-}


data BasicShader = BasicShader { basic_program           :: Program
                                ,basic_vPositionVAD      :: VertexArrayDescriptor Int
                                ,basic_vNormalVAD        :: VertexArrayDescriptor Int
                                ,basic_vTexCoordVAD      :: VertexArrayDescriptor Int
                                ,basic_vPosition         :: AttribLocation
                                ,basic_vNormal           :: AttribLocation
                                ,basic_vTexCoord         :: AttribLocation
                                ,basic_uColor            :: UniformLocation
                                ,basic_uModelMatrix      :: UniformLocation
                                ,basic_uViewMatrix       :: UniformLocation
                                ,basic_uProjectionMatrix :: UniformLocation }

data MyVAO = MyVAO { vao_vao  :: VertexArrayObject
                    ,vao_mode :: PrimitiveMode
                    ,vao_num  :: NumArrayIndices } 

data Resources = Resources { res_basicShader :: BasicShader
                            ,res_paddleVAO   :: MyVAO }
                            {-,elementBuffer   :: BufferObject-}
                            {-,shaders         :: Shaders-}
                            {-,fadeFactor      :: GLfloat }-}


shaderProgramFromPath :: String -> IO (Program)
shaderProgramFromPath name = 
  let basePath = "src/Curve/Client/Render/Shader/"
  in do
  vs :: VertexShader   <- loadShader $ basePath ++ name ++ ".vs"
  fs :: FragmentShader <- loadShader $ basePath ++ name ++ ".fs"
  linkShaderProgram [vs] [fs]


initBasicShader :: IO (BasicShader)
initBasicShader = let 
    fs = sizeOf (undefined::GLfloat)
    stride  = fromIntegral (fs*8)
    posVad  = VertexArrayDescriptor 3 Float stride offset0
    normVad = VertexArrayDescriptor 3 Float stride (plusPtr offset0 (fs*3))
    texVad  = VertexArrayDescriptor 2 Float stride (plusPtr offset0 (fs*6))
  in do 
  p <- shaderProgramFromPath "HelloWorld" {-v1 <- get (attribLocation  p "vPosition")-}
  v1 <- get (attribLocation  p "vPosition")
  v2 <- get (attribLocation  p "vNormal")
  v3 <- get (attribLocation  p "vTexCoord")
  (putStrLn . show) v1
  (putStrLn . show) v2
  (putStrLn . show) v3
  BasicShader p posVad normVad texVad
    <$> get (attribLocation  p "vPosition")
    <*> get (attribLocation  p "vNormal")
    <*> get (attribLocation  p "vTexCoord")
    <*> get (uniformLocation p "uColor")
    <*> get (uniformLocation p "uModelMatrix")
    <*> get (uniformLocation p "uViewMatrix")
    <*> get (uniformLocation p "uProjectionMatrix")

--paddlevao

paddleArray :: [GLfloat]
paddleArray = (concatMap arrayfy) $ zip3 (paddlePosition 1 1 1) paddleNormal paddleTexCoord
  where
   arrayfy ((a,b,c), (d,e,f), (g,h)) = [a,b,c,d,e,f,g,h]
{-paddleArray = (concatMap arr (paddlePosition 1 1 1)) ++ (concatMap arr paddleNormal)-}

arr (a,b,c) = [a,b,c]


paddlePosition :: GLfloat -> GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
paddlePosition x y z = 
  [ ( x, y, z), ( x, y,-z), ( x,-y,-z), ( x,-y, z),
    ( x, y, z), ( x, y,-z), (-x, y,-z), (-x, y, z),
    ( x, y, z), ( x,-y, z), (-x,-y, z), (-x, y, z),
    (-x, y, z), (-x, y,-z), (-x,-y,-z), (-x,-y, z),
    ( x,-y, z), ( x,-y,-z), (-x,-y,-z), (-x,-y, z),
    ( x, y,-z), ( x,-y,-z), (-x,-y,-z), (-x, y,-z) ]

paddleNormal :: [(GLfloat, GLfloat, GLfloat)]
paddleNormal = concatMap (replicate 4)
      [ ( 1, 0, 0),
        ( 0, 1, 0),
        ( 0, 0, 1),
        (-1, 0, 0),
        ( 0,-1, 0),
        ( 0, 0,-1) ]

paddleTexCoord :: [(GLfloat, GLfloat)]
paddleTexCoord = 
  [ ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0),
    ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0),
    ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0),
    ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0),
    ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0),
    ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0) ]

makePaddleVAO :: BasicShader -> IO (VertexArrayObject)
makePaddleVAO s = do
  {-ab  <- makeBuffer ArrayBuffer (concatMap (\(x,y,z) -> [x,y,z]) (paddlePosition 1 1 1))-}
  ab  <- makeBuffer ArrayBuffer paddleArray
  makeVAO $ do
    putStrLn "makeVAO"
    bindBuffer ArrayBuffer $= Just ab
    vertexAttribArray   (basic_vPosition s) $= Enabled
    vertexAttribArray   (basic_vNormal   s) $= Enabled
    vertexAttribArray   (basic_vTexCoord s) $= Enabled
    vertexAttribPointer (basic_vPosition s) $= (ToFloat, (basic_vPositionVAD s))
    vertexAttribPointer (basic_vNormal   s) $= (ToFloat, (basic_vNormalVAD   s))
    vertexAttribPointer (basic_vTexCoord s) $= (ToFloat, (basic_vTexCoordVAD s))
    printError
