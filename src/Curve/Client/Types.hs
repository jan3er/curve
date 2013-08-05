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

-- holds the enviornments state
data Env = Env 
  { _env_playerMap   :: PlayerMap
  , _env_socket      :: Socket
  , _env_nr          :: Int
  , _env_isRunning   :: Bool
  {-, _env_currentTime :: UTCTime-}
  } deriving Show
makeLenses ''Env

----------------------------------
