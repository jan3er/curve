{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Types where

import Data.Time
import Data.Vec

import Control.Lens

-- represends one player
data Player = Player {
      {-playerPosition :: (Float, Float) --replace this with vec-}
      _player_posList :: [(NominalDiffTime, Vec2 Float)]
    } deriving Show
makeLenses ''Player
