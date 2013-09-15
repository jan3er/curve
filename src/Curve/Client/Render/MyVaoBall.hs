{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Client.Render.MyVaoBall
    ( array
    )where


import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=), GLfloat)
import qualified Graphics.GLUtil as GLU
import           Control.Lens

import           Curve.Client.Render.GLTypes


-------------------------------------


array :: [GLfloat]
array = (concatMap toArray) $ zip3 
    (position 1 1 1)
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
