{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Game.Ball where

import           Data.Time
import qualified Data.Vec as V

import Control.Lens

-- represends one player
data Ball = Ball 
    { _ball_referenceTime :: NominalDiffTime
    , _ball_position      :: V.Vec3F
    , _ball_direction     :: V.Vec3F
    , _ball_spin          :: V.Vec3F
    } deriving Show
makeLenses ''Ball

initBall :: Ball
initBall = Ball
    0
    (V.Vec3F 0 0 0)
    (V.Vec3F 0 0 0)
    (V.Vec3F 0 0 0)

getPosition :: NominalDiffTime -> Ball -> Maybe V.Vec3F
getPosition t ball =
    let deltaT :: Float = realToFrac $ ball^.ball_referenceTime - t
    in Just $
                             (ball^.ball_position)
    + V.map (deltaT*)        (ball^.ball_direction)
    + V.map (deltaT*deltaT*) (ball^.ball_spin)

