{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Types where

import Data.Time
{-import Data.Vec-}

import Control.Lens

-- represends one player
data Player = Player {
      _player_posList :: [(NominalDiffTime, Float, Float)]
    } deriving Show
makeLenses ''Player
