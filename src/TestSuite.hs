{-# OPTIONS -Wall #-}

module Main where

import Test.HUnit
import qualified Test.Game.Ball
import qualified Test.Game.Network
import qualified Test.Client.Client

------------------------------------
  
tests :: Test
tests = TestList 
    [ TestLabel "Game.Ball"          Test.Game.Ball.tests
    , TestLabel "Game.Network"       Test.Game.Network.tests
    , TestLabel "Client.Client"      Test.Client.Client.tests]

main :: IO (Counts)
main = runTestTT tests
