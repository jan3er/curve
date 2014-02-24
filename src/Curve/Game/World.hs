{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.World where

import Control.Lens

import qualified Data.Map as Map
import           Data.Map (Map)

import           Curve.Game.Ball
import           Curve.Game.Wall
import           Curve.Game.Player

---------------------------------------

data World = World
    { __ball         :: Ball
    , __extraWalls   :: [Wall]
    , __playerMap    :: Map Int Player
    } deriving Show
makeLenses ''World

---------------------------------------

-- create a new empty world
new :: World
new = World newBall [] Map.empty

---------------------------------------
