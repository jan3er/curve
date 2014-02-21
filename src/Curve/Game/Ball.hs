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
    (M.mkVec3 0 1 0)
    (M.mkVec3 0 0 0)
    1


-- use householder reflection
reflect :: Wall -> NominalDiffTime -> Ball -> Ball
reflect wall t ball=
    let oldDir = ball^._speed
        dotProduct = oldDir `dot` (wall^._normal)
        newDir = oldDir -. ((wall^._normal) *. (2*dotProduct))
    in
    Ball t (positionByTime t ball) newDir (ball^._acceleration) (ball^._size)
    

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


-- TODO: return infinite list instead?

-- get the wall the ball touches next
intersectionList :: [Wall] -> Ball -> (Wall, NominalDiffTime)
intersectionList walls ball =
    {-let walls = traceShow walls' walls'-}
    let f wall = (\time -> (wall, time)) <$> intersection wall ball
        tuples :: [(Wall, NominalDiffTime)] = catMaybes $ f <$> walls
    in
    case tuples of
        [] -> error "Curve.Game.Wall intersectionList: the ball touched no wall!"
        xs -> maximumBy (\a b -> compare (a^._2) (b^._2)) xs


-----------------------------------



-- get the moment of intersection with this wall 
intersection :: Wall -> Ball -> Maybe NominalDiffTime
intersection wall ball = 
    let 
        speedAtTime t = (ball^._speed) +. ((ball^._acceleration) *. t)
        posAtTime t = (ball^._position) +. (speedAtTime t *. t)
        isArriving t = (speedAtTime t) `dot` (wall^._normal) < 0
        -- get all possible intersections
        isValid t = (t > 0) && (Wall.isInRectangle wall (posAtTime t)) && (isArriving t)
        times = filter isValid $ intersectionHelper (wall^._normal) (wall^._center) ball

    -- if there are results, get the minimum
    in do
    deltaTime <- minimumMay times
    return $ ball^._referenceTime + realToFrac deltaTime


--get list of possible intersection-times
intersectionHelper :: Vec3 Float -> Vec3 Float -> Ball -> [Float]
intersectionHelper wallNormal wallCenter ball =
    let 
        {-isSmall x = abs x < 0.0000001-}
        -- position of the ball if wall was in center
        relativePos = (ball^._position) -. wallCenter

        -- offset for the ball's size
        ballOffset  = wallNormal *. (ball^._size)

        --     intersection with plane at time t
        -- <=> (pos + t*dir + t*t*accel) dot normal = 0
        -- <=> t*t*(accel dot normal) + t*(dir dot normal) + (pos dot normal) = 0
        -- <=> a*t^2 + b*t + c = 0 
        --     with
        --       a = accel dot normal
        --       b = dir dot normal
        --       c = pos dot normal
        a  = (ball^._acceleration)       `dot` wallNormal
        b  = (ball^._speed)              `dot` wallNormal
        c1 = (relativePos +. ballOffset) `dot` wallNormal
        c2 = (relativePos -. ballOffset) `dot` wallNormal
    in 
    concat $ map (\(x,y) -> [x,y]) $ catMaybes
         -- TODO: replace == 0 with eps
         [ if (a == 0) then ((\x -> (x,x)) <$> solveLinear b c1) else (solveSquare a b c1)
         , if (a == 0) then ((\x -> (x,x)) <$> solveLinear b c2) else (solveSquare a b c2) ]
        

solveLinear :: Float -> Float -> Maybe Float
solveLinear b c = 
    if (b == 0) then Nothing
                else Just (-c/b)

-- from stack overflow
-- expects a to be nonzero
solveSquare :: Float -> Float -> Float -> Maybe (Float, Float)
solveSquare a b c = if d < 0 then
                Nothing
                else Just (x1, x2)
                    where x1 = e + sqrt d / (2 * a)
                          x2 = e - sqrt d / (2 * a)
                          d = b * b - 4 * a * c
                          e = - b / (2 * a)
