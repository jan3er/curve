{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Game.Ball where

import Safe
import Data.Maybe
import Data.List
import Data.Time
import Data.Aeson.TH
import Control.Lens
import Control.Applicative

import qualified Curve.Game.Math as M
import Curve.Game.Math hiding (map, maximum, zipWith, head)

import Curve.Game.Wall as Wall

import Curve.Game.Network()

-----------------------------------

data Ball = Ball
    { __referenceTime :: NominalDiffTime
    , __position      :: Vec3 Float
    , __direction     :: Vec3 Float
    , __acceleration  :: Vec3 Float
    , __speed         :: Float
    , __size          :: Float
    } deriving Show
makeLenses ''Ball
deriveJSON defaultOptions ''Ball

-----------------------------------

someRandomBall :: Ball
someRandomBall = Ball
    0
    (M.mkVec3 0 0 0)
    (M.normalize $ M.mkVec3 (-1) 1 0)
    (M.mkVec3 0 0 0)
    2
    0.3

-------------------------------------------------------------------------------
-- interface for ball list ----------------------------------------------------
-------------------------------------------------------------------------------


addBall :: Ball -> [Ball] -> [Ball]
addBall ball balls = balls ++ [ball]

currentBall :: [Ball] -> Ball
currentBall = head  
    
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

--only valid for times greater than difftime
positionAtTime :: NominalDiffTime -> Ball -> Vec3 Float
positionAtTime  t ball =
    let deltaT:: Float = realToFrac $ t - ball^._referenceTime
    in if t < 0 then (error "Game.Ball.positionAtTime") else 
                             (ball^._position)
    + M.map (deltaT*)        ((ball^._direction) *. (ball^._speed))
    + M.map (deltaT*deltaT*) (ball^._acceleration)

-----------------------------------

-- reflect the ball before the wall
reflect :: Wall -> NominalDiffTime -> Ball -> Ball
reflect wall t ball=

    let deltaT        = realToFrac $ t - ball^._referenceTime
        oldVelocity   = ((ball^._direction)    *. (ball^._speed))
                      + ((ball^._acceleration) *. deltaT)
        dotProduct    = oldVelocity `dot` (wall^._normal)
        newDirection  = M.normalize (oldVelocity -. ((wall^._normal) *. (2*dotProduct)))
        newPosition   = projectBeforeWall wall (positionAtTime t ball) (ball^._size)
    in
    Ball 
    { __referenceTime = t
    , __position      = newPosition
    , __direction     = newDirection
    , __acceleration  = (ball^._acceleration)
    , __speed         = (ball^._speed) 
    , __size          = (ball^._size) }
    
-----------------------------------

-- get the wall the ball touches next
intersectList :: Ball -> [(Wall, a)] -> (NominalDiffTime, a)
intersectList ball walls =
    let tupleFmap (maybeTime, a) = maybeTime >>= (flip . curry) Just a
        intersections = mapMaybe tupleFmap
                      . map (_1 %~ (momentOfIntersection ball))
    in
    case intersections walls of
        [] -> error "Curve.Game.Wall intersectionList: the ball touched no wall!"
        xs -> minimumBy (\a b -> compare (a^._1) (b^._1)) xs


-------------------------------------------------------------------------------
-- intersection with single walls ---------------------------------------------
-------------------------------------------------------------------------------

-- get the moment of intersection with this wall 
momentOfIntersection :: Ball -> Wall -> Maybe NominalDiffTime
momentOfIntersection ball wall = 
    let 
        isMovingAway = (ball^._direction) `dot` (wall^._normal) < 0
        time = case intersectInfinitePlane (wall^._normal) (wall^._center) ball of
            Left ()         -> if isMovingAway then Just 0 else Nothing
            Right maybeTime -> maybeTime 
    in (\t -> ball^._referenceTime + realToFrac t) <$> time


-- if the ball lies behind the plane return Left
-- otherwise a positive value is returned if the ball will hit the wall in the future
-- nothing if the ball is moving away from the wall
intersectInfinitePlane :: Vec3 Float -> Vec3 Float -> Ball -> Either () (Maybe Float)
intersectInfinitePlane wallNormal wallCenter ball =
    let 
        --     intersection with plane at time t
        -- <=> (pos + t*vel + t*t*accel) dot normal = 0
        -- <=> t*t*(accel dot normal) + t*(vel dot normal) + (pos dot normal) = 0
        -- <=> a*t^2 + b*t + c = 0 
        --     with
        --         a = accel    dot normal
        --         b = velocity dot normal
        --         c = pos      dot normal
        a = (ball^._acceleration)                  `dot` wallNormal
        b = ((ball^._direction) *. (ball^._speed)) `dot` wallNormal
        c = ((ball^._position) -. wallCenter)      `dot` wallNormal - ball^._size
    in 
    if c < 0 
    then 
        Left () 
    else 
        Right $ do
        times <- (\(x,y) -> [x,y]) <$> solveQuadratic a b c
        minimumMay $ filter (>= 0) times
        

-- solve quadratic equation a*x^2 + b*x + c = 0
solveQuadratic :: Float -> Float -> Float -> Maybe (Float, Float)
solveQuadratic a b c  
    | small a && small b && small c = Just (0,0)
    | small a && small b            = Nothing
    | small a                       = Just (-c/b, -c/b)
    | d < 0                         = Nothing
    | otherwise                     = Just (x1, x2)
    where x1 = e + sqrt d / (2 * a)
          x2 = e - sqrt d / (2 * a)
          d = b * b - 4 * a * c
          e = - b / (2 * a)
          small x = abs x < 0.0001
