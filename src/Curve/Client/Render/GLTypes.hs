{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Client.Render.GLTypes where

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


import qualified Curve.Game.Math as M
import           Curve.Game.Math (Vec3, Vec4, Mat33, Mat44)

-------------------------------------


data BasicShader = BasicShader 
    { _basic_program           :: GL.Program
    , _basic_vPositionVAD      :: GL.VertexArrayDescriptor Int
    , _basic_vNormalVAD        :: GL.VertexArrayDescriptor Int
    , _basic_vTexCoordVAD      :: GL.VertexArrayDescriptor Int
    , _basic_vPosition         :: GL.AttribLocation
    , _basic_vNormal           :: GL.AttribLocation
    , _basic_vTexCoord         :: GL.AttribLocation
    , _basic_uColor            :: GL.UniformLocation
    , _basic_uModelMatrix      :: GL.UniformLocation
    , _basic_uViewMatrix       :: GL.UniformLocation
    , _basic_uProjectionMatrix :: GL.UniformLocation
    }
makeLenses ''BasicShader


data MyVao = MyVao 
    { vao_vao  :: GL.VertexArrayObject
    , vao_mode :: GL.PrimitiveMode
    , vao_num  :: GL.NumArrayIndices
    } 
makeLenses ''MyVao


-------------------------------------

makeVao :: BasicShader -> [GLfloat] -> IO (MyVao)
makeVao s array = do
    vao <- GLU.makeVAO $ do
        ab  <- GLU.makeBuffer GL.ArrayBuffer array
        GL.bindBuffer GL.ArrayBuffer $= Just ab
        GL.vertexAttribArray   (s^.basic_vPosition) $= GL.Enabled
        GL.vertexAttribArray   (s^.basic_vNormal  ) $= GL.Enabled
        GL.vertexAttribArray   (s^.basic_vTexCoord) $= GL.Enabled
        GL.vertexAttribPointer (s^.basic_vPosition) $= (GL.ToFloat, (s^.basic_vPositionVAD))
        GL.vertexAttribPointer (s^.basic_vNormal  ) $= (GL.ToFloat, (s^.basic_vNormalVAD  ))
        GL.vertexAttribPointer (s^.basic_vTexCoord) $= (GL.ToFloat, (s^.basic_vTexCoordVAD))
        GLU.printError
    return $ MyVao vao GL.Quads (4*6)
