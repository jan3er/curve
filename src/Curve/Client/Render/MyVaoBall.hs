{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Client.Render.MyVaoBall
    ( array
    )where

import Control.Applicative
import Graphics.Rendering.OpenGL (GLfloat)
import Curve.Game.Math as M

-------------------------------------

--arraybuffer of a sphere
array :: Integer -> GLfloat -> [GLfloat]
array resolution radius = 
    let toArray ((a,b,c), (d,e,f), (g,h)) = [a,b,c,d,e,f,g,h]
    in concat $ toArray <$> zip3 
    (toSphere (position radius) resolution resolution)
    (toSphere normal            resolution resolution)
    (toSphere texCoord          resolution resolution)

--create list of vertex information for whole sphere (for either normal, texCoord or position)
toSphere :: (Integer -> Integer -> Integer -> Integer -> a) -> Integer -> Integer -> [a]
toSphere transf uMax vMax = concat $ do
    --for all u,v
    u <- [0..(uMax-1)]
    v <- [0..(vMax-1)]
    --generate quads of certain types
    let u' = (u+1) `mod` uMax
    let v' = (v+1) `mod` vMax
    return [ transf u  v  uMax vMax 
           , transf u' v  uMax vMax
           , transf u' v' uMax vMax
           , transf u  v' uMax vMax ]

--generate normal of sphere for given uv-coordinate
normal :: Integer -> Integer -> Integer -> Integer -> (GLfloat, GLfloat, GLfloat)
normal u v uMax vMax = M.mkTuple3 
                     . M.normalize 
                     . (\(x,y,z) -> M.mkVec3 x y z) 
                     $ position 1 u v uMax vMax

--generate position on sphere for given uv-coordinate
position :: GLfloat -> Integer -> Integer -> Integer -> Integer -> (GLfloat, GLfloat, GLfloat)
position size u v uMax vMax =
    let fracU = 2 * pi * ((fromInteger u)/(fromInteger uMax))
        fracV = 2 * pi * ((fromInteger v)/(fromInteger vMax))
        (x,y) = (sin fracU, cos fracU)
        (a,b) = (sin fracV, cos fracV)
    in (size*x*a, size*y*a, size*b)

--generate texture coordinate on sphere for given uv-coordinate
texCoord :: Integer -> Integer -> Integer -> Integer -> (GLfloat, GLfloat)
texCoord u v uMax vMax = 
    let fracU = (fromInteger u)/(fromInteger uMax)
        fracV = (fromInteger v)/(fromInteger vMax)
    in (fracU, fracV)
