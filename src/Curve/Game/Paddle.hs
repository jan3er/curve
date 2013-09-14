{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Paddle where

import qualified Data.List as L
import           Data.Time
import           Control.Lens

type Entry = (NominalDiffTime, Float, Float)

data Paddle = Paddle {
      _paddle_positions :: [Entry]
    } deriving Show
makeLenses ''Paddle

clamp :: Paddle -> Paddle
clamp = paddle_positions %~ L.take 3

insert :: Entry -> Paddle -> Paddle
insert entry = paddle_positions %~ (entry:)
