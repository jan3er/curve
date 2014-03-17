{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.Env where 

import Control.Lens

import Curve.Game.World
import Curve.Server.Timer
import Curve.Server.ClientHandle
{-import Curve.Game.Player-}

----------------------------------------

-- holds the enviornments state
data Env = Env 
    { _env_clients     :: [ClientHandle]
    , _env_world       :: World
    , _env_isRunning   :: Bool
    , _env_timer       :: STimer
    } deriving Show
makeLenses ''Env

initEnv :: IO Env
initEnv = do
    timer <- initTimer
    return Env 
        { _env_clients   = []
        , _env_world     = initEmptyWorld
        , _env_isRunning = False
        , _env_timer     = timer }
