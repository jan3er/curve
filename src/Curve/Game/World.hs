{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.World where

import Control.Lens
import Data.Time
import Data.Maybe
import Safe

import qualified Data.Map as Map
import Data.Map (Map)

import Curve.Game.Ball
import Curve.Game.Wall
import Curve.Game.Player

---------------------------------------


data World = World
    { __balls        :: [Ball]          -- ^ the current ball is placed first
    , __extraWalls   :: [Wall]          -- ^ all non-player walls
    , __playerMap    :: Map Int Player
    } deriving Show
makeLenses ''World

---------------------------------------

-- create a new empty world
new :: World
new = World [newBall] [] Map.empty

---------------------------------------


--drop all balls that are no longer relevant
truncBalls :: NominalDiffTime -> [Ball] -> [Ball]
truncBalls currentTime balls = fromMaybe balls $ do
    let isActive ball = currentTime > ball^._referenceTime
    activeBall  <- lastMay (takeWhile isActive balls)
    return $ activeBall : (dropWhile isActive balls)

--
addBall :: Ball -> [Ball] -> [Ball]
addBall ball balls = balls ++ [ball]

currentBall :: [Ball] -> Ball
currentBall = head  
