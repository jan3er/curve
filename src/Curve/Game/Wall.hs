{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Game.Wall where

import           Data.Time
import qualified Data.Vec as V
import           Data.Vec ( (:.) )
import           Debug.Trace

import Control.Lens

data Wall = Wall
    { _wall_maybeNr       :: Maybe Int
    , _wall_lowerLeft     :: V.Vec3 Float
    , _wall_lowerRight    :: V.Vec3 Float
    , _wall_upperLeft     :: V.Vec3 Float
    , _wall_upperRight    :: V.Vec3 Float
    } deriving Show
makeLenses ''Wall

