{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Paddle where

import Data.Time
import Control.Lens

data Paddle = Paddle {
      _paddle_positions :: [(NominalDiffTime, Float, Float)]
    } deriving Show
makeLenses ''Paddle
