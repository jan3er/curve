{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Client.Render.Renderer (initResources, renderStep, Resources) where

import           Control.Concurrent
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Lens

import           Data.List
import qualified Data.Map as Map
import           Data.IORef
import           Data.Vec hiding (map, get)

import           Foreign.Storable (sizeOf)
import           Foreign.Ptr
import           Graphics.GLUtil.Camera3D (deg2rad)

import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL as GL hiding (get, perspective)
import qualified Graphics.Rendering.OpenGL as GL (get)
{-import           Graphics.Rendering.OpenGL.Raw-}
import           Graphics.GLUtil
import           Graphics.GLUtil.VertexArrayObjects

{-import qualified Data.Vec as Vec-}

import           Curve.Client.Types
import           Curve.Game.Types




-----------------------------------------------------------
-- TODO shader programming:
{-http://www.arcadianvisions.com/blog/?p=224-}


--TYPES------------------------

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

drawMyVAO :: MyVAO -> IO ()
drawMyVAO v = withVAO (vao_vao v) $ do drawArrays (vao_mode v) 0 (vao_num v)


data Resources = Resources { res_basicShader :: BasicShader
                            ,res_paddleVAO   :: MyVAO }
                            {-,elementBuffer   :: BufferObject-}
                            {-,shaders         :: Shaders-}
                            {-,fadeFactor      :: GLfloat }-}

--SHADER------------------------

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
  p <- shaderProgramFromPath "HelloWorld"
  BasicShader p posVad normVad texVad
    <$> GL.get (attribLocation  p "vPosition")
    <*> GL.get (attribLocation  p "vNormal")
    <*> GL.get (attribLocation  p "vTexCoord")
    <*> GL.get (uniformLocation p "uColor")
    <*> GL.get (uniformLocation p "uModelMatrix")
    <*> GL.get (uniformLocation p "uViewMatrix")
    <*> GL.get (uniformLocation p "uProjectionMatrix")

--VAO-------------------------------

paddleArray :: [GLfloat]
paddleArray = (concatMap toArray) $ zip3 (paddlePosition 1 1 1) paddleNormal paddleTexCoord
  where toArray ((a,b,c), (d,e,f), (g,h)) = [a,b,c,d,e,f,g,h]

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

makePaddleVAO :: BasicShader -> IO (MyVAO)
makePaddleVAO s = do
  vao <- makeVAO $ do
    ab  <- makeBuffer ArrayBuffer paddleArray
    bindBuffer ArrayBuffer $= Just ab
    vertexAttribArray   (basic_vPosition s) $= Enabled
    vertexAttribArray   (basic_vNormal   s) $= Enabled
    vertexAttribArray   (basic_vTexCoord s) $= Enabled
    vertexAttribPointer (basic_vPosition s) $= (ToFloat, (basic_vPositionVAD s))
    vertexAttribPointer (basic_vNormal   s) $= (ToFloat, (basic_vNormalVAD   s))
    vertexAttribPointer (basic_vTexCoord s) $= (ToFloat, (basic_vTexCoordVAD s))
    printError
  return $ MyVAO vao Quads (4*6)

-----------------------------------------------
--Render!


render :: Resources -> Env -> IO ()
render res env = do
  clearColor $= Color4 0 0.1 0.1 1
  clear [ColorBuffer, DepthBuffer]
  depthMask $= Enabled
  depthFunc $= Just Lequal
  let s = res_basicShader res
  currentProgram $= Just (basic_program s)
  
  let posList = (map (_player_posList . fst . snd)) (Map.toList $ _env_playerMap env)
  
  uniformVec (basic_uColor s)      $= [1,0,1]
  uniformMat (basic_uViewMatrix s) $= [[  1,   0,   0,   0],
                                       [  0,   1,   0,   0],
                                       [  0,   0,   1,   0],
                                       [  0,   0,   0,   1]]

  {-let now = env^.env_timer^.timer_now-}
  {-let deg = realToFrac now-}
  let deg = 42;
  sequence_ $ map (foodoo deg ) posList



  {-putStrLn "--------" -}
  {-putStrLn $ show posList-}
  {-putStrLn "--------" -}
  {-putStrLn $ "--------- " ++ (show $ _env_id env) ++ " -------------"-}
  {-(putStrLn . show) env-}

  {-let (b1, b2) = _env_paddlePos env-}
  {-let (b1, b2) = (200, 300)-}
  {-let (a1, a2) = (realToFrac b1*0.01, realToFrac b2*(-0.01))-}
  {-putStrLn $ (show a1) ++ (show a2)-}



  {-let trans = (translation (fromList [0, 0, -10]) :: Mat44 GLfloat)-}
  {-let rot   = (rotationY a1) `multmm` (rotationX a2) :: Mat44 GLfloat-}
  {-uniformMat (basic_uModelMatrix      s) $= (matToLists) (trans `multmm` rot)-}
  {-uniformMat (basic_uProjectionMatrix s) $= (matToLists) (perspective 0.01 100 (deg2rad 30) 1 :: Mat44 GLfloat)-}

  {-drawMyVAO (res_paddleVAO res)-}

  GLFW.swapBuffers

  where 
    foodoo deg l = do
      let s = res_basicShader res
      let (a1, a2) = case l of
                        [] -> (200, 200)
                        (_,x:.y:.()) : _ -> (realToFrac x*0.01, realToFrac y*(-0.01))

      let trans = (translation (fromList [0, 0, -10]) :: Mat44 GLfloat)
      let rot   = (rotationY (a1 + deg)) `multmm` (rotationX (a2 + deg)) :: Mat44 GLfloat
      uniformMat (basic_uModelMatrix      s) $= (matToLists) (trans `multmm` rot)
      uniformMat (basic_uProjectionMatrix s) $= (matToLists) (perspective 0.01 100 (deg2rad 30) 1 :: Mat44 GLfloat)

      drawMyVAO (res_paddleVAO res)


-----------------------------------------------------------------------------
--PUBLIC---------------------------------------------------------------------
-----------------------------------------------------------------------------

initResources :: IO (Resources)
initResources = do 
  s <- initBasicShader
  vao <- makePaddleVAO s
  return $ Resources s vao

renderStep :: Env -> StateT Resources IO ()
renderStep env = do
    res <- get
    liftIO $ render res env
    return ()


