{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Curve.Client.Render.Renderer where

import           Control.Concurrent
import           Control.Applicative
import           Data.Foldable
import           Data.List
import           Data.Vec hiding (map, get)

import           Foreign.Storable (sizeOf)
import           Graphics.GLUtil.Camera3D (deg2rad)

import           Graphics.UI.GLUT (swapBuffers)
import           Graphics.Rendering.OpenGL hiding (perspective)
import           Graphics.GLUtil
import           Graphics.GLUtil.VertexArrayObjects

{-import qualified Data.Vec as Vec-}

import           Curve.Client.Types


data Shaders = Shaders { vertexShader      :: VertexShader
                        ,fragmentShader    :: FragmentShader
                        ,prog              :: Program
                        ,uColor            :: UniformLocation
                        ,uModelMatrix      :: UniformLocation
                        ,uViewMatrix       :: UniformLocation
                        ,uProjectionMatrix :: UniformLocation
                        ,vPosition         :: AttribLocation }

{-data Resources = Resources { vertexBuffer  :: BufferObject-}
                            {-,elementBuffer :: BufferObject-}
                            {-,shaders       :: Shaders-}
                            {-,fadeFactor    :: GLfloat }-}


vertexBufferData :: [GLfloat]
vertexBufferData = [-1, -1, 1, -1, -1, 1, 1, 1]

cube :: GLfloat -> [GLfloat]
cube w = (Data.List.concat . map (\(a,b,c) -> [a,b,c]) )
      {-[ (-w,-w, w), ( w,-w, w), (w, w, w), (-w, w, w)]-}
      [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
        ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
        ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
        (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
        ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
        ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]
 
elementBufferData :: [GLuint]
elementBufferData = [0..3]

initShaders = do vs <- loadShader "src/Curve/Client/Render/Shader/HelloWorld.vs"
                 fs <- loadShader "src/Curve/Client/Render/Shader/HelloWorld.fs"
                 p <- linkShaderProgram [vs] [fs]
                 Shaders vs fs p
                   <$> get (uniformLocation p "uColor")
                   <*> get (uniformLocation p "uModelMatrix")
                   <*> get (uniformLocation p "uViewMatrix")
                   <*> get (uniformLocation p "uProjectionMatrix")
                   <*> get (attribLocation  p "vPosition")

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


fooVAO :: GLfloat -> Shaders  -> IO (VertexArrayObject)
fooVAO w s = do
  ab  <- makeBuffer ArrayBuffer (cube w)

  let vPos = vPosition s
  let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 3
  let vad = VertexArrayDescriptor 3 Float stride offset0

  makeVAO $ do
    bindBuffer ArrayBuffer   $= Just ab
    vertexAttribPointer vPos $= (ToFloat, vad)
    vertexAttribArray vPos   $= Enabled




draw :: IO ()
draw = do
  s <- initShaders
  clearColor $= Color4 0 0 0 1
  clear [ColorBuffer]
  currentProgram $= Just (prog s)

  --set color
  uniformVec (uColor s)      $= [1,0,1]
  uniformMat (uViewMatrix s) $= [[  1,   0,   0,   0],
                                 [  0,   1,   0,   0],
                                 [  0,   0,   1,   0],
                                 [  0,   0,   0,   1]]


  let trans = (translation (fromList [0, 0, -10]) :: Mat44 GLfloat)
  let rot   = (rotationX 1) `multmm` (rotationY 1)
  uniformMat (uModelMatrix      s) $= (matToLists) (trans `multmm` rot)
  uniformMat (uProjectionMatrix s) $= (matToLists) (perspective 0.01 100 (deg2rad 30) 1 :: Mat44 GLfloat)

  vao2 <- fooVAO 1 s
  vao3 <- fooVAO 2 s
  withVAO vao3 $ do drawArrays Quads 0 (4*6)

  {-drawArrays Quads 0 (4*6)-}

  swapBuffers



display :: MVar Env -> IO ()
display envar = do
  {-clear [ ColorBuffer ]-}
  {-flush-}
  draw

