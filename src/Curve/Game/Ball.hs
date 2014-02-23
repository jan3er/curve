{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Game.Ball where

import           Safe
import           Data.Maybe
import           Data.List
import           Data.Time
{-import           Debug.Trace-}
import           Control.Lens
import           Control.Applicative

import qualified Curve.Game.Math as M
import           Curve.Game.Math hiding (map, maximum)

import           Curve.Game.Wall as Wall

-----------------------------------

data Ball = Ball
    { __referenceTime :: NominalDiffTime
    , __position      :: Vec3 Float
    , __speed         :: Vec3 Float
    , __acceleration  :: Vec3 Float
    , __size          :: Float
    } deriving Show
makeLenses ''Ball

-- PUBLIC ---------------------------------

newBall :: Ball
newBall = Ball
    0
    (M.mkVec3 0 0 0)
    (M.mkVec3 11 8 0)
    (M.mkVec3 0 17 0)
    1


-- reflect the ball before the wall
reflect :: Wall -> NominalDiffTime -> Ball -> Ball
reflect wall t ball=
    let oldDir = ball^._speed
        dotProduct = oldDir `dot` (wall^._normal)
        newDir = oldDir -. ((wall^._normal) *. (2*dotProduct))
        newPos = projectBeforeWall wall (positionByTime t ball) (ball^._size)
    in
    Ball t newPos newDir (ball^._acceleration) (ball^._size)
    

--only valid for times greater than difftime
positionByTime :: NominalDiffTime -> Ball -> Vec3 Float
positionByTime  t ball =
    let deltaT :: Float = realToFrac $ t - ball^._referenceTime
        {-foo x = trace (show deltaT) x-}
        {-foo x = x-}
    {-in foo $ Just $-}
    in if t < 0 then (error "Game.Ball.positionByTime") else 
                             (ball^._position)
    + M.map (deltaT*)        (ball^._speed)
    + M.map (deltaT*deltaT*) (ball^._acceleration)


-----------------------------------

-- get the wall the ball touches next
intersectList :: [Wall] -> Ball -> (Wall, NominalDiffTime)
intersectList walls ball =
    let f wall = (\time -> (wall, time)) <$> intersectWall wall ball
        tuples :: [(Wall, NominalDiffTime)] = catMaybes $ f <$> walls
    in
    case tuples of
        [] -> error "Curve.Game.Wall intersectionList: the ball touched no wall!"
        xs -> minimumBy (\a b -> compare (a^._2) (b^._2)) xs


-- get the moment of intersection with this wall 
intersectWall :: Wall -> Ball -> Maybe NominalDiffTime
intersectWall wall ball = 
    let 
        {-speedAtTime t = (ball^._speed) +. ((ball^._acceleration) *. t)-}
        {-posAtTime t = (ball^._position) +. (speedAtTime t *. t)-}
        {-isArriving t = (speedAtTime t) `dot` (wall^._normal) < 0-}
        {-isValid t = (Wall.isInRectangle wall (posAtTime t)) && (isArriving t)-}
        {-isValid t = True-}

        time = case intersectInfinitePlane (wall^._normal) (wall^._center) ball of
            Left () -> Just 0
            Right maybeTime -> maybeTime 

    in (\t -> ball^._referenceTime + realToFrac t) <$> time


-- if the ball lies behind the plane return Left
-- otherwise a positive value is returned if the ball will hit the wall in the future
-- nothing if the ball is moving away from the wall
intersectInfinitePlane :: Vec3 Float -> Vec3 Float -> Ball -> Either () (Maybe Float)
intersectInfinitePlane wallNormal wallCenter ball =
    let 
        relativePos = (ball^._position) -. wallCenter
        
        --     intersection with plane at time t
        -- <=> (pos + t*dir + t*t*accel) dot normal = 0
        -- <=> t*t*(accel dot normal) + t*(dir dot normal) + (pos dot normal) = 0
        -- <=> a*t^2 + b*t + c = 0 
        --     with
        --       a = accel dot normal
        --       b = dir dot normal
        --       c = pos dot normal
        a = (ball^._acceleration)       `dot` wallNormal
        b = (ball^._speed)              `dot` wallNormal
        c = relativePos `dot` wallNormal - ball^._size
    in 
    if c < 0 
    then 
        Left () 
    else Right $ do
        times <- (\(x,y) -> [x,y]) <$> solveQuadratic a b c
        minimumMay $ filter (>= 0) times
        

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
