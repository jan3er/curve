{-# OPTIONS -Wall #-}

module Main where

import Test.HUnit
import qualified Test.Game.Ball


------------------------------------
  
tests :: Test
tests = TestList 
    [ TestLabel "Game.Ball" Test.Game.Ball.tests]

main :: IO (Counts)
main = runTestTT tests
