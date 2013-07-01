{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}

module Curve.Game.Types where

import Data.Time
import Network.Socket

-- will hold the enviornment state
data Env = Env {
   envPlayers   :: [Player]
} deriving Show

-- represends one player
data Player = Player {
      playerPosition :: (Float, Float) --replace this with vec
    } deriving Show

