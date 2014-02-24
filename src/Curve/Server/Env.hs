{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.Env where 

import           Control.Lens
import           Control.Applicative
import qualified Data.Map as Map

import qualified Curve.Game.World as World
import           Curve.Game.World (World)

{-import qualified Curve.Game.Ball as Ball-}
{-import           Curve.Game.Ball (Ball)-}

{-import qualified Curve.Game.Wall as Wall-}
{-import           Curve.Game.Wall (Wall)-}

import qualified Curve.Server.Timer as Timer
import           Curve.Server.Timer (Timer)

import           Curve.Server.ClientMap
import           Curve.Game.Player

----------------------------------------

-- holds the enviornments state
data Env = Env 
    { _env_clientMap   :: ClientMap
    , _env_world       :: World
    , _env_isRunning   :: Bool
    , _env_timer       :: Timer
    } deriving Show
makeLenses ''Env

initEnv :: IO Env
initEnv = Env <$> pure Map.empty
              <*> pure World.new
              <*> pure False
              <*> Timer.new
