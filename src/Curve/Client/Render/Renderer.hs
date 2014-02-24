{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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

import           Foreign.Storable (sizeOf)
import           Foreign.Ptr

import qualified Graphics.UI.GLFW as GLFW

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=), GLfloat)

import qualified Graphics.GLUtil as GLU
import qualified Graphics.GLUtil.Camera3D as GLU
{-import qualified Graphics.GLUtil.VertexArrayObjects as GLU-}


import           Curve.Client.Render.GLTypes
import qualified Curve.Client.Render.MyVaoPaddle as PaddleVao
import qualified Curve.Client.Render.MyVaoWall as WallVao
import qualified Curve.Client.Render.MyVaoBall as BallVao

import qualified Curve.Game.Math as M
import           Curve.Game.Math (Vec3, Vec4, Mat33, Mat44)
import           Curve.Client.Types
import qualified Curve.Client.Timer as Timer

import           Curve.Game.Ball   as Ball 
import           Curve.Game.Player as Player
import           Curve.Game.Paddle as Paddle
import           Curve.Game.Wall   as Wall
import           Curve.Game.World  as World


-----------------------------------------------------------
-- TODO shader programming:
{-http://www.arcadianvisions.com/blog/?p=224-}


--TYPES------------------------

data Resources = Resources 
    { _res_basicShader      :: BasicShader
    , _res_vaoPaddle        :: MyVao
    , _res_vaoWall          :: MyVao
    , _res_vaoBall          :: MyVao
    }
makeLenses ''Resources

drawMyVao :: MyVao -> IO ()
drawMyVao v = GLU.withVAO (vao_vao v) $ do GL.drawArrays (vao_mode v) 0 (vao_num v)



--SHADER------------------------

shaderProgramFromPath :: String -> IO (GL.Program)
shaderProgramFromPath name = 
    let basePath = "src/Curve/Client/Render/Shader/"
    in do
    vs :: GL.Shader   <- GLU.loadShader GL.VertexShader $ basePath ++ name ++ ".vs"
    fs :: GL.Shader <- GLU.loadShader GL.FragmentShader $ basePath ++ name ++ ".fs"
    GLU.linkShaderProgram [vs, fs]


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

-----------------------------------------------
--Render!


render :: Resources -> Env -> IO ()
render res env = 
    let foodoo deg l = do
        let s = res^.res_basicShader
        let (a1, a2) = case l of
                        [] -> (200, 200)
                        (_,x,y) : _ -> (realToFrac x*0.01, realToFrac y*(-0.01))

        let trans = (M.translation (M.fromList [0, 0, -10]) :: M.Mat44 GLfloat)
        let rot   = (M.rotationY (a1 + deg)) `M.multmm` (M.rotationX (a2 + deg)) :: M.Mat44 GLfloat
        GLU.uniformMat (s^.basic_uModelMatrix     ) $= (M.matToLists) (trans `M.multmm` rot)

        GLU.uniformMat (s^.basic_uProjectionMatrix) $= matToGLLists (getProjectionMatrix $ env^.env_window^.window_size)

        drawMyVao (res^.res_vaoPaddle)

    in do
    GL.clearColor $= GL.Color4 0 0.1 0.1 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.depthMask $= GL.Enabled
    GL.depthFunc $= Just GL.Lequal
    let s = res^.res_basicShader
    GL.currentProgram $= Just (s^.basic_program)

    let posList = ((\x -> x^._2^._paddle^._positions )) <$> (Map.toList $ env^.env_world^._playerMap )

    GLU.uniformVec (s^.basic_uColor)      $= [1,0,1]
    GLU.uniformMat (s^.basic_uViewMatrix) $= (matToGLLists . M.translation) (0 M.:. 0 M.:. (-40))

    {-let now = env^.env_timer^.timer_now-}
    {-let deg = realToFrac now-}
    let deg = 42;
    sequence_ $ map (foodoo deg ) posList

    --------------------------------
    
    GL.currentProgram $= Just (s^.basic_program)
    let ballPos = positionAtTime (Timer.getTime $ env^.env_timer) (env^.env_world^._ball)
    GLU.uniformMat (s^.basic_uProjectionMatrix) $= (matToGLLists.getProjectionMatrix) (env^.env_window^.window_size)
    GLU.uniformMat (s^.basic_uModelMatrix)      $= (matToGLLists.M.translation)       ballPos
    GLU.uniformMat (s^.basic_uViewMatrix)       $= (matToGLLists.M.translation)       (M.mkVec3 0 0 (-20))

    drawMyVao (res^.res_vaoBall)


    -- draw arena
    sequence_ $ 
        (\wall -> do
            fooDrawMyVao s (wallTransformationMatrix wall) (res^.res_vaoWall)
        ) <$> (env^.env_world^._extraWalls)
        


    GLFW.swapBuffers


-----------------------------------------------------------------------------
--STUFF----------------------------------------------------------------------
-----------------------------------------------------------------------------

matToGLLists :: M.Mat44 Float -> [[GLfloat]]
matToGLLists m = 
    M.matToLists $ M.map (M.map realToFrac) m


getProjectionMatrix :: GL.Size -> M.Mat44 Float
getProjectionMatrix (GL.Size x y) =
    let near  = 0.01
        far   = 1000
        ratio = (realToFrac x) / (realToFrac y)
        angle = Prelude.maximum[1, 1/ratio] * GLU.deg2rad 30
    in M.perspective near far angle ratio


fooDrawMyVao :: BasicShader -> Mat44 Float -> MyVao -> IO ()
fooDrawMyVao s modelMatrix vao = do
    GL.currentProgram $= Just (s^.basic_program)
    GLU.uniformMat (s^.basic_uModelMatrix) $= matToGLLists modelMatrix
    drawMyVao vao


-- calculate transformation matrix to display a wall 
wallTransformationMatrix :: Wall -> Mat44 Float
wallTransformationMatrix wall =
    let (width, height) = wall^._dimensions
        scaleMat        = (M.mkVec4 width height 1 1) `M.scale` M.identity
        rotMat          = M.from33to44 $ fromJust $ M.invert $ 
                          M.mkVec3 ((wall^._updir) `M.cross` (wall^._normal)) (wall^._updir) (wall^._normal)
        transMat        = M.translation (wall^._center)
    in transMat `M.multmm` rotMat `M.multmm` scaleMat
        
-----------------------------------------------------------------------------
--PUBLIC---------------------------------------------------------------------
-----------------------------------------------------------------------------

initResources :: IO (Resources)
initResources = do 
  shader    <- initBasicShader
  vaoPaddle <- makeVao shader PaddleVao.array
  vaoWall   <- makeVao shader WallVao.array
  vaoBall   <- makeVao shader (BallVao.array 42 1)
  return $ Resources shader vaoPaddle vaoWall vaoBall


renderStep :: Env -> StateT Resources IO ()
renderStep env = do
    res <- get
    liftIO $ render res env


