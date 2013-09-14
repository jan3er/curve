{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.World where

import Control.Lens

import qualified Data.Map as Map
import           Data.Map (Map)

import qualified Curve.Game.Ball as Ball
import           Curve.Game.Ball (Ball)

{-import qualified Curve.Game.Wall as Wall-}
import           Curve.Game.Wall (Wall)

{-import qualified Curve.Game.Player as Player-}
import           Curve.Game.Player (Player)

---------------------------------------

data World = World
    { world_ball         :: Ball
    , world_extraWalls   :: [Wall]
    , world_playerMap    :: Map Int Player
    } deriving Show
makeLenses ''World

---------------------------------------

-- create a new empty world
new :: World
new = World Ball.new [] Map.empty

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
