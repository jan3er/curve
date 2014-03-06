{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Timer where

import Data.Time

---------------------------------------

class Timer a where
    getTime :: a -> NominalDiffTime
    --TODO: maybe ioUpdate :: Timer -> IO Timer
