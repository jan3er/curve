{-# OPTIONS -Wall #-}

module Main where

import Test.HUnit
import qualified Test.Game.Ball
import qualified Test.Game.Network


------------------------------------
  
tests :: Test
tests = TestList 
    [ TestLabel "Game.Ball"    Test.Game.Ball.tests
    , TestLabel "Game.Network" Test.Game.Network.tests]

main :: IO (Counts)
main = runTestTT tests
