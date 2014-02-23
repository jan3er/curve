{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Client.Render.MyVaoBall
    ( array
    )where


import Graphics.Rendering.OpenGL (GLfloat)
import Control.Applicative
import Curve.Game.Math as M
-------------------------------------


array :: [GLfloat]
array = 
    let x = 100
    in (concatMap toArray) $ zip3 
    (toSphere pos   x x)
    (toSphere norma x x)
    (toSphere coord x x)
    where toArray ((a,b,c), (d,e,f), (g,h)) = [a,b,c,d,e,f,g,h]


toSphere :: (Integer -> Integer -> Integer -> Integer -> a) -> Integer -> Integer -> [a]
toSphere transf uMax vMax = concat $ do
    u <- [0..(uMax-1)]
    v <- [0..(vMax-1)]
    return $ toQuad transf u v uMax vMax

toQuad :: (Integer -> Integer -> Integer -> Integer -> a) -> Integer -> Integer -> Integer -> Integer -> [a]
toQuad transf u v uMax vMax = 
    let u' = (u+1) `mod` uMax
        v' = (v+1) `mod` vMax
    in [ transf u  v  uMax vMax 
       , transf u' v  uMax vMax
       , transf u' v' uMax vMax
       , transf u  v' uMax vMax ]

coord :: Integer -> Integer -> Integer -> Integer -> (GLfloat, GLfloat)
coord u v uMax vMax = 
    let fracU = (fromInteger u)/(fromInteger uMax)
        fracV = (fromInteger v)/(fromInteger vMax)
    in (fracU, fracV)

norma :: Integer -> Integer -> Integer -> Integer -> (GLfloat, GLfloat, GLfloat)
norma u v uMax vMax = M.mkTuple3 
                   . M.normalize 
                   . (\(x,y,z) -> M.mkVec3 x y z) 
                   $ pos u v uMax vMax

pos :: Integer -> Integer -> Integer -> Integer -> (GLfloat, GLfloat, GLfloat)
pos u v uMax vMax =
    let fracU = 2 * pi * ((fromInteger u)/(fromInteger uMax))
        fracV = 2 * pi * ((fromInteger v)/(fromInteger vMax))
        (x,y) = (sin fracU, cos fracU)
        (a,b) = (sin fracV, cos fracV)
    in (x*a, y*a, b)
