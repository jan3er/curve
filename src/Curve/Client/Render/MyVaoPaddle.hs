{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Client.Render.MyVaoPaddle 
    ( array
    )where


import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=), GLfloat)
import qualified Graphics.GLUtil as GLU
import           Control.Lens

import           Curve.Client.Render.GLTypes

import qualified Curve.Game.Paddle as Paddle
import           Curve.Game.Paddle (Paddle)


-------------------------------------


array :: [GLfloat]
array = (concatMap toArray) $ zip3 
    ((\(x, y) -> position x y 0.1) (both %~ realToFrac $ Paddle.dimensions)) 
    normal
    texCoord
    where toArray ((a,b,c), (d,e,f), (g,h)) = [a,b,c,d,e,f,g,h]

-- creates a cube around the center with dimensions 2x 2y 2z
position :: GLfloat -> GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
position x y z = 
    [ ( x, y, z), ( x, y,-z), ( x,-y,-z), ( x,-y, z),
      ( x, y, z), ( x, y,-z), (-x, y,-z), (-x, y, z),
      ( x, y, z), ( x,-y, z), (-x,-y, z), (-x, y, z),
      (-x, y, z), (-x, y,-z), (-x,-y,-z), (-x,-y, z),
      ( x,-y, z), ( x,-y,-z), (-x,-y,-z), (-x,-y, z),
      ( x, y,-z), ( x,-y,-z), (-x,-y,-z), (-x, y,-z) ]

normal :: [(GLfloat, GLfloat, GLfloat)]
normal = concatMap (replicate 4)
    [ ( 1, 0, 0),
      ( 0, 1, 0),
      ( 0, 0, 1),
      (-1, 0, 0),
      ( 0,-1, 0),
      ( 0, 0,-1) ]

texCoord :: [(GLfloat, GLfloat)]
texCoord = 
    [ ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0),
      ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0),
      ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0),
      ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0),
      ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0),
      ( 0, 0), ( 0, 1), ( 1, 1), ( 1, 0) ]
