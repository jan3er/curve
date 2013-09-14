{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.Env where 

import           Control.Lens
import           Control.Applicative
import qualified Data.Map as Map

import qualified Curve.Game.Ball as Ball
import           Curve.Game.Ball (Ball)

import qualified Curve.Game.Wall as Wall
import           Curve.Game.Wall (Wall)

import qualified Curve.Server.Timer as Timer
import           Curve.Server.Timer (Timer)

import           Curve.Server.PlayerMap

----------------------------------------

-- holds the enviornments state
data Env = Env 
    { _env_playerMap   :: PlayerMap
    , _env_isRunning   :: Bool
    , _env_timer       :: Timer
    , _env_ball        :: Ball
    } deriving Show
makeLenses ''Env


initEnv :: IO Env
initEnv = Env <$> pure Map.empty
              <*> pure False
              <*> Timer.init
              <*> pure Ball.init
