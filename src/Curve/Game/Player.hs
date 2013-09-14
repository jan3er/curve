{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Player where

import Data.Time

import Control.Lens

-- represends one player
data Player = Player {
      _player_posList :: [(NominalDiffTime, Float, Float)]
    } deriving Show
makeLenses ''Player
