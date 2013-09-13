{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Client.Render.Renderer (initResources, renderStep, Resources) where

{-import           Control.Concurrent-}
import           Control.Applicative
{-import           Control.Monad-}
import           Control.Monad.State
import           Control.Lens

{-import           Data.List-}
import           Data.Maybe
import qualified Data.Map as Map
{-import           Data.IORef-}
import qualified Data.Vec as V

import           Foreign.Storable (sizeOf)
import           Foreign.Ptr

import qualified Graphics.UI.GLFW as GLFW

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=), GLfloat)

import qualified Graphics.GLUtil as GLU
import qualified Graphics.GLUtil.Camera3D as GLU
{-import qualified Graphics.GLUtil.VertexArrayObjects as GLU-}


import           Curve.Client.Types
import qualified Curve.Client.Timer as Timer
import           Curve.Game.Player
import           Curve.Game.Ball


-----------------------------------------------------------
-- TODO shader programming:
{-http://www.arcadianvisions.com/blog/?p=224-}


--TYPES------------------------

data BasicShader = BasicShader { basic_program           :: GL.Program
                               , basic_vPositionVAD      :: GL.VertexArrayDescriptor Int
                               , basic_vNormalVAD        :: GL.VertexArrayDescriptor Int
                               , basic_vTexCoordVAD      :: GL.VertexArrayDescriptor Int
                               , basic_vPosition         :: GL.AttribLocation
                               , basic_vNormal           :: GL.AttribLocation
                               , basic_vTexCoord         :: GL.AttribLocation
                               , basic_uColor            :: GL.UniformLocation
                               , basic_uModelMatrix      :: GL.UniformLocation
                               , basic_uViewMatrix       :: GL.UniformLocation
                               , basic_uProjectionMatrix :: GL.UniformLocation
                               }

data MyVAO = MyVAO { vao_vao  :: GL.VertexArrayObject
                   , vao_mode :: GL.PrimitiveMode
                   , vao_num  :: GL.NumArrayIndices
                   } 

data Resources = Resources { res_basicShader      :: BasicShader
                           , res_paddleVAO        :: MyVAO
                           }
                            {-,elementBuffer   :: BufferObject-}
                            {-,shaders         :: Shaders-}
                            {-,fadeFactor      :: GLfloat }-}

drawMyVAO :: MyVAO -> IO ()
drawMyVAO v = GLU.withVAO (vao_vao v) $ do GL.drawArrays (vao_mode v) 0 (vao_num v)



--SHADER------------------------

shaderProgramFromPath :: String -> IO (GL.Program)
shaderProgramFromPath name = 
    let basePath = "src/Curve/Client/Render/Shader/"
    in do
    vs :: GL.VertexShader   <- GLU.loadShader $ basePath ++ name ++ ".vs"
    fs :: GL.FragmentShader <- GLU.loadShader $ basePath ++ name ++ ".fs"
    GLU.linkShaderProgram [vs] [fs]


initBasicShader :: IO (BasicShader)
initBasicShader = 
    let fs = sizeOf (undefined::GLfloat)
        stride  = fromIntegral (fs*8)
        posVad  = GL.VertexArrayDescriptor 3 GL.Float stride GLU.offset0
        normVad = GL.VertexArrayDescriptor 3 GL.Float stride (plusPtr GLU.offset0 (fs*3))
        texVad  = GL.VertexArrayDescriptor 2 GL.Float stride (plusPtr GLU.offset0 (fs*6))
    in do 
    p <- shaderProgramFromPath "HelloWorld"
    BasicShader p posVad normVad texVad
        <$> GL.get (GL.attribLocation  p "vPosition")
        <*> GL.get (GL.attribLocation  p "vNormal")
        <*> GL.get (GL.attribLocation  p "vTexCoord")
        <*> GL.get (GL.uniformLocation p "uColor")
        <*> GL.get (GL.uniformLocation p "uModelMatrix")
        <*> GL.get (GL.uniformLocation p "uViewMatrix")
        <*> GL.get (GL.uniformLocation p "uProjectionMatrix")

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
    vao <- GLU.makeVAO $ do
        ab  <- GLU.makeBuffer GL.ArrayBuffer paddleArray
        GL.bindBuffer GL.ArrayBuffer $= Just ab
        GL.vertexAttribArray   (basic_vPosition s) $= GL.Enabled
        GL.vertexAttribArray   (basic_vNormal   s) $= GL.Enabled
        GL.vertexAttribArray   (basic_vTexCoord s) $= GL.Enabled
        GL.vertexAttribPointer (basic_vPosition s) $= (GL.ToFloat, (basic_vPositionVAD s))
        GL.vertexAttribPointer (basic_vNormal   s) $= (GL.ToFloat, (basic_vNormalVAD   s))
        GL.vertexAttribPointer (basic_vTexCoord s) $= (GL.ToFloat, (basic_vTexCoordVAD s))
        GLU.printError
    return $ MyVAO vao GL.Quads (4*6)

-----------------------------------------------
--Render!


render :: Resources -> Env -> IO ()
render res env = 
    let foodoo deg l = do
          let s = res_basicShader res
          let (a1, a2) = case l of
                            [] -> (200, 200)
                            (_,x,y) : _ -> (realToFrac x*0.01, realToFrac y*(-0.01))

          let trans = (V.translation (V.fromList [0, 0, -10]) :: V.Mat44 GLfloat)
          let rot   = (V.rotationY (a1 + deg)) `V.multmm` (V.rotationX (a2 + deg)) :: V.Mat44 GLfloat
          GLU.uniformMat (basic_uModelMatrix      s) $= (V.matToLists) (trans `V.multmm` rot)

          GLU.uniformMat (basic_uProjectionMatrix s) $= V.matToLists (getProjectionMatrix $ env^.env_window^.window_size)

          drawMyVAO (res_paddleVAO res)

    in do
    GL.clearColor $= GL.Color4 0 0.1 0.1 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.depthMask $= GL.Enabled
    GL.depthFunc $= Just GL.Lequal
    let s = res_basicShader res
    GL.currentProgram $= Just (basic_program s)

    let posList = (map (_player_posList . fst . snd)) (Map.toList $ _env_playerMap env)

    GLU.uniformVec (basic_uColor s)      $= [1,0,1]
    GLU.uniformMat (basic_uViewMatrix s)       $= (matToGLLists . V.translation) (0 V.:. 0 V.:. (-20))

    {-let now = env^.env_timer^.timer_now-}
    {-let deg = realToFrac now-}
    let deg = 42;
    sequence_ $ map (foodoo deg ) posList

    --------------------------------
    
    let ballPos = fromJust $ getPosition (Timer.getTime $ env^.env_timer) (env^.env_ball)
    GLU.uniformMat (basic_uProjectionMatrix s) $= V.matToLists (getProjectionMatrix $ env^.env_window^.window_size)
    GLU.uniformMat (basic_uModelMatrix s)      $= (matToGLLists . V.translation) ballPos
    GLU.uniformMat (basic_uViewMatrix s)       $= (matToGLLists . V.translation) (0 V.:. 0 V.:. (-20))

    drawMyVAO (res_paddleVAO res)


    GLFW.swapBuffers


-----------------------------------------------------------------------------
--STUFF----------------------------------------------------------------------
-----------------------------------------------------------------------------

matToGLLists :: V.Mat44 Float -> [[GLfloat]]
matToGLLists m = 
    V.matToLists $ V.map (V.map realToFrac) m

getProjectionMatrix :: GL.Size -> V.Mat44 GLfloat
getProjectionMatrix (GL.Size x y) =
    let near  = 0.01
        far   = 1000
        ratio = (realToFrac x) / (realToFrac y)
        angle = Prelude.maximum[1, 1/ratio] * GLU.deg2rad 30
    in V.perspective near far angle ratio

-----------------------------------------------------------------------------
--PUBLIC---------------------------------------------------------------------
-----------------------------------------------------------------------------

initResources :: IO (Resources)
initResources = do 
  shader <- initBasicShader
  vao    <- makePaddleVAO shader
  return $ Resources shader vao

renderStep :: Env -> StateT Resources IO ()
renderStep env = do
    res <- get
    liftIO $ render res env


