{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Game.Ball where

import           Safe
import           Data.Maybe
import           Data.List
import           Data.Time
import           Debug.Trace
import           Control.Lens
import           Control.Applicative

import qualified Curve.Game.Math as M
import           Curve.Game.Math hiding (map, maximum, zipWith)

import           Curve.Game.Wall as Wall

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

-- PUBLIC ---------------------------------

newBall :: Ball
newBall = Ball
    0
    (M.mkVec3 0 3 0)
    (M.normalize $ M.mkVec3 1 1 0)
    (M.mkVec3 0 20 0)
    20
    0.3

truncBallList :: NominalDiffTime -> [Ball] -> [Ball]
truncBallList currentTime balls = fromMaybe balls $ do
    --TODO maybe assert nonempty
    let isActive ball = currentTime > ball^._referenceTime
    activeBall  <- lastMay (takeWhile isActive balls)
    return $ activeBall : (dropWhile isActive balls)
    

-- reflect the ball before the wall
reflect :: Wall -> NominalDiffTime -> Ball -> Ball
reflect wall t ball=
    let oldVelocity  = (velocityAtTime t ball)
        dotProduct   = oldVelocity `dot` (wall^._normal)
        newDirection = M.normalize (oldVelocity -. ((wall^._normal) *. (2*dotProduct)))
        newPosition  = projectBeforeWall wall (positionAtTime t ball) (ball^._size)
    in
    Ball 
        t 
        newPosition 
        newDirection
        (ball^._acceleration)
        (ball^._speed)
        (ball^._size)
    

--only valid for times greater than difftime
positionAtTime :: NominalDiffTime -> Ball -> Vec3 Float
positionAtTime  t ball =
    let deltaT:: Float = realToFrac $ t - ball^._referenceTime
        {-deltaT = trace (show deltaT') deltaT'-}
    in if t < 0 then (error "Game.Ball.positionAtTime") else 
                             (ball^._position)
    + M.map (deltaT*)        ((ball^._direction) *. (ball^._speed))
    + M.map (deltaT*deltaT*) (ball^._acceleration)

--only valid for times greater than difftime
velocityAtTime :: NominalDiffTime -> Ball -> Vec3 Float
velocityAtTime t ball =
    let deltaT:: Float = realToFrac $ t - ball^._referenceTime
        {-deltaT = trace (show deltaT') deltaT'-}
    in if t < 0 then (error "Game.Ball.velocityAtTime") else 
      ((ball^._direction) *. (ball^._speed))
    -- TODO why does squared look so much better?
    {-+ ((ball^._acceleration) *. deltaT *. deltaT)-}
    + ((ball^._acceleration) *. deltaT)


-----------------------------------

-- get the wall the ball touches next
intersectList :: [Wall] -> Ball -> (Int, Wall , NominalDiffTime)
intersectList walls ball =
    let
        tupleFmap idx wall maybeTime = fmap (\time -> (idx, wall, time)) maybeTime
        intersections = catMaybes
                      $ zipWith3 tupleFmap
                        [0..] walls (flip intersectWall ball <$> walls)
    in
    case intersections of
        [] -> error "Curve.Game.Wall intersectionList: the ball touched no wall!"
        xs -> minimumBy (\a b -> compare (a^._3) (b^._3)) xs


-- get the moment of intersection with this wall 
intersectWall :: Wall -> Ball -> Maybe NominalDiffTime
intersectWall wall ball = 
    let 
        {-velocityAtTime t = (ball^._di1rection) +. ((ball^._acc2eleration) *. t)-}
        {-posAtTime t = (ball^._pos2ition) +. (velo2cityAtTime t *. t)-}
        {-isArriving t = (velo1cityAtTime t) `dot` (wall^._normal) < 0-}
        {-isValid t = (Wall.isInRectangle wall (posAtTime t)) && (isArriving t)-}
        isValidBehind = (ball^._direction) `dot` (wall^._normal) < 0

        time = case intersectInfinitePlane (wall^._normal) (wall^._center) ball of
            Left () -> if isValidBehind then Just 0 else Nothing
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
        -- <=> (pos + t*vel + t*t*accel) dot normal = 0
        -- <=> t*t*(accel dot normal) + t*(vel dot normal) + (pos dot normal) = 0
        -- <=> a*t^2 + b*t + c = 0 
        --     with
        --       a = accel    dot normal
        --       b = velocity dot normal
        --       c = pos      dot normal
        a = (ball^._acceleration)                  `dot` wallNormal
        b = ((ball^._direction) *. (ball^._speed)) `dot` wallNormal
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
