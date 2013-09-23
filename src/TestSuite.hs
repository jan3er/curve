{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           System.Exit (exitFailure)
import           Control.Lens

import           Curve.Game.Math as M
import           Curve.Game.Wall as Wall
import           Curve.Game.Ball as Ball

main :: IO ()
main = do
    putStrLn "---------------------"
    ballWallTest
    putStrLn "---------------------"
    exitFailure



aBall :: Ball
aBall = Ball
    0                   --ref time
    (M.mkVec3 0 0 0)    --pos
    (M.mkVec3 1 0 0)    --speed
    (M.mkVec3 0 0 0)    --accel
    0                   --size

aWall :: Wall
aWall = Wall
    (M.mkVec3 (-1) 0 0) --normal
    (M.mkVec3   0  0 1) --up
    (M.mkVec3   4  0 0) --center
    (1,1)               --dimension

ballWallTest :: IO ()
ballWallTest = do
    let newPos ball t  = (ball^._position)
                    +. ((ball^._speed)        *. t)
                    +. ((ball^._acceleration) *. (t*t))

    let t = intersectionHelper (aWall^._normal) (aWall^._center) aBall
    putStrLn $ "intersectionHelper: " ++ show t
    putStrLn $ "intersection: " ++ (show $ intersection aWall aBall)
    putStrLn $ show $ newPos aBall 4
    putStrLn $ show $ isInRectangle  aWall (mkVec3 4 0 0) 
    return ()

