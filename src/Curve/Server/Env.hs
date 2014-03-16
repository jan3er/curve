{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.Env where 

import Control.Lens

import qualified Curve.Game.World as World
import Curve.Game.World (World)

{-import qualified Curve.Game.Ball as Ball-}
{-import Curve.Game.Ball (Ball)-}

{-import qualified Curve.Game.Wall as Wall-}
{-import Curve.Game.Wall (Wall)-}

import qualified Curve.Server.Timer as Timer
import Curve.Server.Timer (STimer)

import Curve.Server.ClientMap
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
    timer <- Timer.new
    return Env 
        { _env_clients   = []
        , _env_world     = World.new
        , _env_isRunning = False
        , _env_timer     = timer }
