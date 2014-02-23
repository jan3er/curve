{-# OPTIONS -Wall #-}

module Test.Game.Ball where

import Test.HUnit

import Curve.Game.Math as M
import Curve.Game.Wall as Wall
import Curve.Game.Ball as Ball

------------------------------------

-- the simplest scenario
test_intersectWall0 :: Test
test_intersectWall0 = TestCase $ do 
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
    assertEqual "" (Just 10) (intersectWall wall ball)

-- rotated plane
test_intersectWall1 :: Test
test_intersectWall1 = TestCase $ do 
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
    assertEqual "" (Just 4) (intersectWall wall ball)

-- nonzero acceleration
test_intersectWall2 :: Test
test_intersectWall2 = TestCase $ do 
    let wall = Wall 
            (M.mkVec3 0 0 (-1)) 
            (M.mkVec3 1 0 0) 
            (M.mkVec3 0 0 0) 
            (1,1)
    let ball = Ball 
            2
            (M.mkVec3 0 0 (-111)) 
            (M.mkVec3 0 0 1) 
            (M.mkVec3 0 0 1) 
            1
    assertEqual "" (Just 12) (intersectWall wall ball)

-- leaving the plane
-- TODO this might need to be changed to Nothing
test_intersectWall3 :: Test
test_intersectWall3 = TestCase $ do 
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
    assertEqual "" (Just 1) (intersectWall wall ball)

test_intersectWall :: Test
test_intersectWall = TestList 
    [ TestLabel "" test_intersectWall0
    , TestLabel "" test_intersectWall1
    , TestLabel "" test_intersectWall2
    , TestLabel "" test_intersectWall3]

-----------------------------------------


test_intersectList0 :: Test
test_intersectList0 = TestCase $ do
    let wall = Wall
            (M.mkVec3 0 0 (-1))
            (M.mkVec3 1 0 0)
            (M.mkVec3 0 0 0)
            (1,1)
    let ball = Ball
            2
            (M.mkVec3 0 0 (-111))
            (M.mkVec3 0 0 1)
            (M.mkVec3 0 0 1)
            1
    -- TODO instance Eq
    assertEqual "" 12 (snd $ intersectList [wall] ball)

test_intersectList:: Test
test_intersectList = TestList 
    [ TestLabel "" test_intersectList0]

-----------------------------------------

tests :: Test
tests = TestList 
    [ TestLabel "intersectWall" test_intersectWall
    , TestLabel "intersectList" test_intersectList]
