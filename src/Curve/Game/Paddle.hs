{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Paddle where

import qualified Data.List as L
import           Data.Time
import           Control.Lens

type Entry = (NominalDiffTime, Float, Float)

data Paddle = Paddle {
      _positions :: [Entry]
    } deriving Show
makeLenses ''Paddle

clamp :: Paddle -> Paddle
clamp = positions %~ L.take 3

insert :: Entry -> Paddle -> Paddle
insert entry = positions %~ (entry:)

-- x and y dimensions of the paddle
dimensions :: (Float, Float)
dimensions = (1, 1)
