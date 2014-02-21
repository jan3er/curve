{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.HUnit

{-import           System.Exit (exitFailure)-}
{-import           Control.Lens-}
{-import           Control.Applicative-}
{-import           Control.Monad-}
{-import           Debug.Trace-}

import           Curve.Game.Math as M
import           Curve.Game.Wall as Wall
import           Curve.Game.Ball as Ball

import           Test.Game.Wall

------------------------------------


-- the simplest scenario
test0 :: Test
test0 = TestCase $ do 
    let wall = Wall 
            (M.mkVec3 0 0 (-1)) 
            (M.mkVec3 1 0 0) 
            (M.mkVec3 0 0 0) 
            (1,1)
    let ball = Ball 
            0
            (M.mkVec3 0 0 (-11)) 
            (M.mkVec3 0 0 1) 
            (M.mkVec3 0 0 0) 
            1
    assertEqual "" (Just 10) (intersection wall ball)

-- rotated plane
test1 :: Test
test1 = TestCase $ do 
    let wall = Wall 
            (M.normalize $ M.mkVec3 (-1) (-1) 0) 
            (M.mkVec3 0 0 1) 
            (M.mkVec3 0 0 0) 
            (1,1)
    let ball = Ball 
            0
            (M.mkVec3 (-10) 0 0) 
            (M.mkVec3 1 1 0) 
            (M.mkVec3 0 0 0) 
            (sqrt 2)
    assertEqual "" (Just 4) (intersection wall ball)

-- nonzero acceleration
test2 :: Test
test2 = TestCase $ do 
    let wall = Wall 
            (M.mkVec3 0 0 (-1)) 
            (M.mkVec3 1 0 0) 
            (M.mkVec3 0 0 0) 
            (1,1)
    let ball = Ball 
            0
            (M.mkVec3 0 0 (-111)) 
            (M.mkVec3 0 0 1) 
            (M.mkVec3 0 0 1) 
            1
    assertEqual "" (Just 10) (intersection wall ball)

-- leaving the plane
test3 :: Test
test3 = TestCase $ do 
    let wall = Wall 
            (M.mkVec3 0 0 1) 
            (M.mkVec3 1 0 0) 
            (M.mkVec3 0 0 0) 
            (1,1)
    let ball = Ball 
            0
            (M.mkVec3 0 0 1) 
            (M.mkVec3 0 0 1) 
            (M.mkVec3 0 0 0) 
            10
    assertEqual "" (Nothing) (intersection wall ball)


tests = TestList 
    [ TestLabel "test0" test0
    , TestLabel "test1" test1
    , TestLabel "test2" test2
    , TestLabel "test3" test3]

main :: IO ()
main = do
    _ <- runTestTT tests
    return ()
