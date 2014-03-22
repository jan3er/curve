{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Timer where

import Data.Time

---------------------------------------

class Timer a where
    -- get the game-time
    getTime :: a -> NominalDiffTime
    -- to be called at the beginning of a round
    setReferenceTime :: NominalDiffTime -> a -> a
    -- update the internal time of the timer
    ioUpdate :: a -> IO a
