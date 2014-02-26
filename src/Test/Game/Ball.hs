{-# OPTIONS -Wall #-}

module Test.Game.Ball where

import Test.HUnit

import Control.Lens
import Control.Applicative

import qualified Curve.Game.Math as M
import Curve.Game.Math (Vec3)

import Curve.Game.Wall as Wall
import Curve.Game.Ball as Ball

------------------------------------

-- used to compare floating point numbers for closeness
-- TODO: put in Utils module
data Close = Close Float 
    deriving (Show)
instance Eq Close where
    Close x == Close y = abs (x - y) < eps

eps :: Float 
eps = 0.00001

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
            1
    (Just 10) @=? (intersectWall wall ball)

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
            1
            (sqrt 2)
    (Just 4) @=? (intersectWall wall ball)

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
            1
    (Just 12) @=? (intersectWall wall ball)

-- leaving the plane
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
            1
            10
    (Nothing) @=? (intersectWall wall ball)

test_intersectWall :: Test
test_intersectWall = TestList 
    [ TestLabel "test_intersectWall0" test_intersectWall0
    , TestLabel "test_intersectWall1" test_intersectWall1
    , TestLabel "test_intersectWall2" test_intersectWall2
    , TestLabel "test_intersectWall3" test_intersectWall3]

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
            1
    12 @=? (intersectList [wall] ball)^._3

test_intersectList:: Test
test_intersectList = TestList 
    [ TestLabel "test_intersectList0" test_intersectList0]

-----------------------------------------

reflectionTest :: Wall -> Ball -> Ball -> Float -> Vec3 Float -> Assertion
reflectionTest wall ballIn ballExpect time posAtTime = do
    -- check time and posAtTime
    assertEqual "time" (Just $ Close time) ((Close . realToFrac) <$> intersectWall wall ballIn)
    assertEqual "posAtTime" (M.map Close posAtTime) (M.map Close $ positionAtTime 10 ballIn)
    -- check reflected ball
    let ballReflect = reflect wall 10 ballIn
    assertEqual "direction"    (M.map Close $ ballExpect^._direction)    (M.map Close $ ballReflect^._direction)
    assertEqual "position"     (M.map Close $ ballExpect^._position)     (M.map Close $ ballReflect^._position)
    assertEqual "acceleration" (M.map Close $ ballExpect^._acceleration) (M.map Close $ ballReflect^._acceleration)
    assertEqual "speed"        (      Close $ ballExpect^._speed)        (      Close $ ballReflect^._speed)
    assertEqual "size"         (      Close $ ballExpect^._size)         (      Close $ ballReflect^._size)

-- without acceleration
test_reflect0 :: Test
test_reflect0 = TestCase $ do
    let wall = Wall
            (M.mkVec3 (-1) 0 0)
            (M.mkVec3 0 0 1)
            (M.mkVec3 11 0 0)
            (1,1)
    let ballIn = Ball
            0
            (M.mkVec3 (0) (-10) 0)
            (M.normalize $ M.mkVec3 1 1 0)
            (M.mkVec3 0 0 0)
            (sqrt 2)
            1
    let ballExpect = Ball
            0
            (M.mkVec3 (9.9) 0 0)
            (M.normalize $ M.mkVec3 (-1) 1 0)
            (M.mkVec3 0 0 0)
            (sqrt 2)
            1
    reflectionTest wall ballIn ballExpect 10 (M.mkVec3 10 0 0)
    

-- with acceleration
test_reflect1 :: Test
test_reflect1 = TestCase $ do
    let wall = Wall
            (M.mkVec3 (-1) 0 0)
            (M.mkVec3 0 0 1)
            (M.mkVec3 11 0 0)
            (1,1)
    let ballIn = Ball
            0
            (M.mkVec3 (0) (-20) 0)
            (M.normalize $ M.mkVec3 1 0 0)
            (M.mkVec3 0 0.2 0)
            1
            1
    let ballExpect = Ball
            0
            (M.mkVec3 (9.9) 0 0)
            (M.normalize $ M.mkVec3 (-1) 2 0)
            (M.mkVec3 0 0.2 0)
            1
            1
    reflectionTest wall ballIn ballExpect 10 (M.mkVec3 10 0 0)



test_reflect :: Test
test_reflect = TestList 
    [ TestLabel "test_reflect0" test_reflect0
    , TestLabel "test_reflect1" test_reflect1]

-----------------------------------------


tests :: Test
tests = TestList 
    [ TestLabel "intersectWall" test_intersectWall
    , TestLabel "intersectList" test_intersectList
    , TestLabel "reflect"       test_reflect]
