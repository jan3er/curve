{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Client.Types where 

import           Data.Time
import qualified Data.Map as Map
import           Data.List
import           Network.Socket

import           Curve.Network.Types
import           Curve.Game.Types

import           Control.Category
import           Data.Label
import           Prelude hiding ((.), id)



----------------------------------------

type PlayerMap = Map.Map Int (Player, Maybe Client)

-- holds the enviornments state
data Env = Env {
  _env_playerMap   :: PlayerMap,
  _env_socket      :: Maybe Socket,
  _env_isRunning   :: Bool
} deriving Show
$(mkLabels [''Env])

----------------------------------

-- creates a new clean env
newEnv :: Env
newEnv = Env { _env_playerMap = Map.empty,
               _env_isRunning = False }
