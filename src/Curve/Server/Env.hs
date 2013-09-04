{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.Env where 

import           Control.Lens

import qualified Curve.Server.Timer as Timer
import           Curve.Server.PlayerMap

----------------------------------------

-- holds the enviornments state
data Env = Env 
    { _env_playerMap   :: PlayerMap
    , _env_isRunning   :: Bool
    , _env_timer       :: Timer.Timer
    } deriving Show
makeLenses ''Env
