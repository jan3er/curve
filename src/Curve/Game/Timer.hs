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

    --TODO: maybe ioUpdate :: Timer -> IO Timer
