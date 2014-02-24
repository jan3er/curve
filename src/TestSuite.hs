{-# OPTIONS -Wall #-}

module Main where

import Test.HUnit
import qualified Test.Game.Ball
import qualified Test.Network.Network


------------------------------------
  
tests :: Test
tests = TestList 
    [ TestLabel "Game.Ball" Test.Game.Ball.tests
    , TestLabel "Network.Network" Test.Network.Network.tests]

main :: IO (Counts)
main = runTestTT tests
