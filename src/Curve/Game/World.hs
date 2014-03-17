{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.World where

import Control.Lens
{-import Control.Applicative-}
import Control.Monad.State
import Control.Applicative
import Data.Time
{-import Data.Tuple-}
import Data.Maybe
import Safe

import qualified Data.Map as Map
import Data.Map (Map)

import Curve.Game.Ball
import Curve.Game.Wall
import Curve.Game.Player
import Curve.Game.Timer

---------------------------------------

data GameStatus = Running
                | NotRunning
                | IDonwKnow
    deriving Show

data World = World
    { __currentTime  :: NominalDiffTime 
    , __balls        :: [Ball]          -- ^ the current ball is placed first
    , __extraWalls   :: [Wall]          -- ^ all non-player walls
    , __players      :: [Player]
    , __gameStatus   :: GameStatus
    } deriving Show
makeLenses ''World

-------------------------------------------------------------------------------
-- initialization -------------------------------------------------------------
-------------------------------------------------------------------------------

-- create a new empty world
initEmptyWorld :: World
initEmptyWorld = World
    { __currentTime  = 0
    , __balls        = []
    , __extraWalls   = []
    , __players      = []
    , __gameStatus   = IDonwKnow }

-- create a new empty world
initWorld :: Int -> World
initWorld noPlayers =
    let (playerWalls, extraWalls) = initArena noPlayers
    in World
    { __currentTime  = 0
    , __balls        = []
    , __extraWalls   = extraWalls
    , __players      = initPlayer <$> playerWalls
    , __gameStatus   = IDonwKnow }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- to be called imedeately before stuff
update :: (Timer t) => t -> World -> World
update timer = execState $ do
    _currentTime .= getTime timer
    _balls       %= truncBalls (getTime timer)

    where
    --drop all balls that are no longer relevant
    truncBalls :: NominalDiffTime -> [Ball] -> [Ball]
    truncBalls currentTime balls = fromMaybe balls $ do
        let isActive ball = currentTime > ball^._referenceTime
        activeBall  <- lastMay (takeWhile isActive balls)
        return $ activeBall : (dropWhile isActive balls)


----------------------------------------

nextImpact :: World -> (NominalDiffTime, (Wall, Maybe Int))
nextImpact world =
    let playerWalls  = zipWith (\nr wall -> (wall,(wall,Just nr))) [0..]
                     . map (view _wall)
                     $ (world^._players)

        extraWalls   = map (\wall -> (wall,(wall,Nothing)))
                       (world^._extraWalls)
    in intersectList 
       (currentBall (world^._balls)) 
       (playerWalls ++ extraWalls)
             

--TODO (this comes next)
--two methods:
--  -to be called at the moment of impact to check the paddle pos
--  -to be called to get the next ball
--  (should be generic enough for server and client)
foo :: World -> ()
foo world =
    let
        (momentOfImpact, (wall, maybeNr)) = nextImpact world
    in
        ()




{-
------------------------

    before the game, when someone joins/leaves 
        broadcast clientMsg: { [all clients], myNr/idx}
        

    ------

    before game is running
        no players at all
        join/leave messages

    when game is running
        client list is immutable //WHY?

    ------
    

    each round is started with a message containing:
        - a start ball
        
    
    each round is finnished with a message containing:

   


    ------


    mainworldmethod, to be called 
        - by server, once the paddle pos for a given time has arrived
          to broadcast the real world state
        - by client to get an approx world state (everytime new info arrives)
    
    provides following information
        - moment of impact
        - uncertain ball | reflected ball | dropped nr

    need following information
        - walls (with maybe id)
        - paddle positions
        - some sort of time, maybe the currentball just is enough (TODO)

 
    mainWorldMethod :: time -> (wall at time, or deduce from time) -> 

    game {
        [walls]
        [players]
        [balls]
        GameStatus: (TODO eg running...)
        TODO: see need following information
        
    }

    wall {
        maybe playerid
    }

    player { 
        playerid
        [positions]
    }

    ~~~~

    serverEnv {
        [client]
    }

    client {
        playerid
    }

------------------------
-}
