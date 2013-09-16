{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Paddle where

import qualified Data.List as L
import           Data.Time
import           Control.Lens

type Entry = (NominalDiffTime, Float, Float)

data Paddle = Paddle {
      __positions :: [Entry]
    } deriving Show
makeLenses ''Paddle

clamp :: Paddle -> Paddle
clamp = _positions %~ L.take 3

insert :: Entry -> Paddle -> Paddle
insert entry = _positions %~ (entry:)

-- x and y dimensions of the paddle
dimensions :: (Float, Float)
dimensions = (1, 1)
