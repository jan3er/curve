{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Game.Ball where

import           Data.Maybe
import           Data.List
import           Data.Time
import           Debug.Trace
import           Control.Lens
import           Control.Applicative

import qualified Curve.Game.Math as M
import           Curve.Game.Math (Vec3)

import qualified Curve.Game.Wall as Wall
import           Curve.Game.Wall (Wall)

-----------------------------------

data Ball = Ball 
    { _ball_referenceTime :: NominalDiffTime
    , _ball_position      :: Vec3 Float
    , _ball_speed         :: Vec3 Float
    , _ball_acceleration  :: Vec3 Float
    , _ball_size          :: Float
    } deriving Show
makeLenses ''Ball

-----------------------------------

new :: Ball
new = Ball
    0
    (M.mkVec3 0 0 0)
    (M.mkVec3 0 0 0)
    (M.mkVec3 0 0 0)
    0

positionByTime :: NominalDiffTime -> Ball -> Maybe (Vec3 Float)
positionByTime  t ball =
    let deltaT :: Float = realToFrac $ t - ball^.ball_referenceTime
        foo x = trace (show deltaT) x
        {-foo x = x-}
    in foo $ Just $
                             (ball^.ball_position)
    + M.map (deltaT*)        (ball^.ball_speed)
    + M.map (deltaT*deltaT*) (ball^.ball_acceleration)



intersectionList :: [Wall] -> Ball -> Wall
intersectionList walls ball =
    let f wall = (\time -> (wall, time)) <$> intersection wall ball
        tuples :: [(Wall, NominalDiffTime)] = catMaybes $ f <$> walls
    in
    case tuples of
        [] -> error "Curve.Game.Wall intersectionList: the ball touched no wall!"
        xs -> fst $ maximumBy (\a b -> compare (a^._2) (b^._2)) xs
    


intersection :: Wall -> Ball -> Maybe NominalDiffTime
intersection wall ball =
    let maybeTime = intersection' wall ball 
        newPos t  = (ball^.ball_position)
                    M.+. ((ball^.ball_speed)        M.*. t)
                    M.+. ((ball^.ball_acceleration) M.*. (t*t))
        
    in do
        t <- maybeTime
        if Wall.isInRectangle wall (newPos t)
            then return $ realToFrac t
            else Nothing
    

-- pretty fucked up. 
-- repeat basic calculations from scrath
-- use foo | guard
--         | guard
intersection' :: Wall -> Ball -> Maybe Float
intersection' wall ball =
    let isSmall x = abs x < 0.000001
        pos       = ball^.ball_position
        speed     = ball^.ball_speed
        accel     = ball^.ball_acceleration
        ballSize  = ball^.ball_size
        normal    = Wall.normal wall
        distance  = Wall.distance wall
    in
    if isSmall $ normal `M.dot` accel
    then 
        if isSmall $ normal `M.dot` speed
        then
             Nothing
        else 
            Just $ ((normal `M.dot` pos) + (distance - ballSize)) / normal `M.dot` speed
    else 
        let p   = (normal `M.dot` speed) / (normal `M.dot` accel)
            q   = (normal `M.dot` pos) + (distance - ballSize) / (normal `M.dot` accel)
            tmp = 0.25*p*p - q
        in
        if tmp < 0
        then 
            Nothing
        else
            if -0.5*p + sqrt tmp < 0
            then
                Just $ -0.5*p - sqrt tmp
            else 
                if -0.5*p - sqrt tmp < 0
                then
                    Just $ -0.5*p + sqrt tmp
                else 
                    Just $ -0.5*p - (abs $ sqrt tmp)
