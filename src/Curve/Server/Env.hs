{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.Env where 

import           Control.Lens
import           Control.Applicative
import qualified Data.Map as Map

import           Curve.Game.Ball
import qualified Curve.Server.Timer as Timer
import           Curve.Server.PlayerMap

----------------------------------------

-- holds the enviornments state
data Env = Env 
    { _env_playerMap   :: PlayerMap
    , _env_isRunning   :: Bool
    , _env_timer       :: Timer.Timer
    , _env_ball        :: Ball
    } deriving Show
makeLenses ''Env


initEnv :: IO Env
initEnv = Env <$> pure Map.empty
              <*> pure False
              <*> Timer.initTime
              <*> pure initBall
