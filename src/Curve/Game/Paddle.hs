{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Paddle where

import qualified Data.List as L
import Data.List
import Data.Time
import Data.Aeson.TH
import Control.Lens

import Curve.Game.Network()

-------------------------

type Entry = (NominalDiffTime, Float, Float)

data Paddle = Paddle {
      __positions :: [Entry]
    , __dimension :: (Float, Float)
    } deriving Show
makeLenses ''Paddle
deriveJSON defaultOptions ''Paddle

-------------------------

initPaddle :: Paddle
initPaddle = Paddle 
    { __positions = []
    , __dimension = (0.1, 0.1) }

clamp :: Paddle -> Paddle
clamp = _positions %~ L.take 3

insert :: Entry -> Paddle -> Paddle
insert entry = _positions %~ (entry:)

-- x and y dimensions of the paddle
dimensions :: (Float, Float)
dimensions = (1, 1)

-- just a stump, TODO
foo :: NominalDiffTime -> Paddle -> ((Float, Float), Bool)
foo  time paddle = 
    let
        maybeSucc = find (\(t,_,_) -> t >= time) (paddle^._positions)
    in case maybeSucc of
        Just (_,x,y) -> ((x,y), True)
        Nothing      -> ((0,0), False)


paddlePosAtTime :: NominalDiffTime -> Paddle -> (Float, Float)
paddlePosAtTime time paddle = fst $ foo time paddle

isCertainAtTime:: NominalDiffTime -> Paddle -> Bool
isCertainAtTime  time paddle = snd $ foo time paddle
