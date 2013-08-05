{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Client.Types where 

import           Data.Time
import qualified Data.Map as Map

import           Network.Socket

import           Control.Lens

import           Curve.Network.Types
import           Curve.Game.Types



----------------------------------------

type PlayerMap = Map.Map Int (Player, Maybe Client)

data Timer = Timer
  { _timer_offset      :: NominalDiffTime
  , _timer_lastQuery   :: UTCTime
  , _timer_waitForResp :: Bool
  , _timer_now         :: NominalDiffTime
  , _timer_start       :: UTCTime
  } deriving Show
makeLenses ''Timer

-- holds the enviornments state
data Env = Env 
  { _env_playerMap     :: PlayerMap
  , _env_socket        :: Socket
  , _env_nr            :: Int
  , _env_isRunning     :: Bool
  , _env_timer         :: Timer
  } deriving Show
makeLenses ''Env

----------------------------------

-- time between two msgTime polls in seconds
queryOffset :: Int
queryOffset = 1
