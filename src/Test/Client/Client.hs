{-# OPTIONS -Wall #-}

module Test.Client.Client where

import Test.HUnit

import Curve.Client.Client
import Curve.Client.Types
import Curve.Game.World as World

--------------------------------------

--TODO
test_dropBall :: Test 
test_dropBall = TestCase $ do 
    assertEqual "" 4 4

--------------------------------------

tests :: Test
tests = TestList 
    [ TestLabel "dropBall" test_dropBall ]
