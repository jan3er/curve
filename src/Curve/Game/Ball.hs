{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Game.Ball where

import           Data.Time
import qualified Data.Vec as V
import           Data.Vec ( (:.) )
import           Debug.Trace

--TODO
{-import Curve.Game.Wall-}

import Control.Lens

-- represends one player
data Ball = Ball 
    { _ball_referenceTime :: NominalDiffTime
    , _ball_position      :: V.Vec3 Float
    , _ball_speed         :: V.Vec3 Float
    , _ball_acceleration  :: V.Vec3 Float
    , _ball_size          :: Float
    } deriving Show
makeLenses ''Ball

initBall :: Ball
initBall = Ball
    0
    (0 V.:. 0 V.:. 0)
    (0 V.:. 0 V.:. 1)
    (0 V.:. 0 V.:. 0)
    0

getPosition :: NominalDiffTime -> Ball -> Maybe (V.Vec3 Float)
getPosition t ball =
    let deltaT :: Float = realToFrac $ t - ball^.ball_referenceTime
        foo x = trace (show deltaT) x
        {-foo x = x-}
    in foo $ Just $
                             (ball^.ball_position)
    + V.map (deltaT*)        (ball^.ball_speed)
    + V.map (deltaT*deltaT*) (ball^.ball_acceleration)

