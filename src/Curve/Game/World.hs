{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.World where

import Control.Lens
import Control.Applicative
import Control.Monad.State
import Data.Time
import Data.Tuple
import Data.Maybe
import Safe

import qualified Data.Map as Map
import Data.Map (Map)

import Curve.Game.Ball
import Curve.Game.Wall
import Curve.Game.Player
import Curve.Game.Timer

---------------------------------------

data World = World
    { __currentTime  :: NominalDiffTime 
    , __balls        :: [Ball]          -- ^ the current ball is placed first
    , __extraWalls   :: [Wall]          -- ^ all non-player walls
    , __playerMap    :: Map Int Player
    } deriving Show
makeLenses ''World

---------------------------------------

-- create a new empty world
new :: World
new = World
    { __currentTime  = 0
    , __balls        = [newBall]
    , __extraWalls   = []
    , __playerMap    = Map.empty
    }

---------------------------------------


update :: (Timer t) => t -> World -> World
update timer = execState $ do
    _currentTime .= getTime timer


--drop all balls that are no longer relevant
truncBalls :: NominalDiffTime -> [Ball] -> [Ball]
truncBalls currentTime balls = fromMaybe balls $ do
    let isActive ball = currentTime > ball^._referenceTime
    activeBall  <- lastMay (takeWhile isActive balls)
    return $ activeBall : (dropWhile isActive balls)

addBall :: Ball -> [Ball] -> [Ball]
addBall ball balls = balls ++ [ball]

currentBall :: [Ball] -> Ball
currentBall = head  

----------------------------------------

--TODO: this should be done next
{-foo :: [Wall] -> PlayerMap -> (Ball, Maybe Int)-}
{-foo walls pm = error "not implemented jet"-}

doo :: World -> (NominalDiffTime, (Wall, Maybe Int))
doo world =
    let playerWalls  = map (\(nr,wall) -> (wall,(wall,Just nr)))
                     . Map.toList
                     . Map.map (view _wall)
                     $ (world^._playerMap) 
        extraWalls   = map (\wall -> (wall,(wall,Nothing)))
                       (world^._extraWalls)
    in intersectList 
       (currentBall (world^._balls)) 
       (playerWalls ++ extraWalls)
             

foo :: World -> ()
foo world =
    let
        (momentOfImpact, (wall, maybeNr)) = doo world
    in
        ()
