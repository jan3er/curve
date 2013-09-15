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
    { _ball         :: Ball
    , _extraWalls   :: [Wall]
    , _playerMap    :: Map Int Player
    } deriving Show
makeLenses ''World

---------------------------------------

-- create a new empty world
new :: World
new = World newBall [] Map.empty

---------------------------------------


{-
world (
    ball

    non-player walls?
    [wall]

    all players (bot and human)
    Map nr player

    player {
        wall
        paddle
    
    )

)

env (
    world
    Map nr client
    
)
-}
