{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Paddle where

import qualified Data.List as L
import Data.List
import Data.Time
import Control.Lens

-------------------------

type Entry = (NominalDiffTime, Float, Float)

data Paddle = Paddle {
      __positions :: [Entry]
    } deriving Show
makeLenses ''Paddle

-------------------------

initPaddle :: Paddle
initPaddle = Paddle 
    { __positions = [] }

clamp :: Paddle -> Paddle
clamp = _positions %~ L.take 3

insert :: Entry -> Paddle -> Paddle
insert entry = _positions %~ (entry:)

-- x and y dimensions of the paddle
dimensions :: (Float, Float)
dimensions = (1, 1)

-- just a stump, TODO
positionAtTime :: NominalDiffTime -> Paddle -> ((Float, Float), Bool)
positionAtTime time paddle = 
    let
        maybeSucc = find (\(t,_,_) -> t >= time) (paddle^._positions)
    in case maybeSucc of
        Just (_,x,y) -> ((x,y), True)
        Nothing      -> ((0,0), False)
