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

-----------------------------------

newBall :: Ball
newBall = Ball
    0
    (M.mkVec3 0 0 0)
    (M.mkVec3 0 0 0)
    (M.mkVec3 0 0 0)
    0

positionByTime :: NominalDiffTime -> Ball -> Maybe (Vec3 Float)
positionByTime  t ball =
    let deltaT :: Float = realToFrac $ t - ball^._referenceTime
        foo x = trace (show deltaT) x
        {-foo x = x-}
    in foo $ Just $
                             (ball^._position)
    + M.map (deltaT*)        (ball^._speed)
    + M.map (deltaT*deltaT*) (ball^._acceleration)


-- get the wall the ball touches next
intersectionList :: [Wall] -> Ball -> (Wall, NominalDiffTime)
intersectionList walls ball =
    let f wall = (\time -> (wall, time)) <$> intersection wall ball
        tuples :: [(Wall, NominalDiffTime)] = catMaybes $ f <$> walls
    in
    case tuples of
        [] -> error "Curve.Game.Wall intersectionList: the ball touched no wall!"
        xs -> maximumBy (\a b -> compare (a^._2) (b^._2)) xs


-- get the moment of intersection with this wall 
intersection :: Wall -> Ball -> Maybe NominalDiffTime
intersection wall ball =
    let deltaTime = maximum $ intersectionHelper (wall^._normal) (wall^._center) ball
        newPos t  = (ball^._position)
                    +. ((ball^._speed)        *. t)
                    +. ((ball^._acceleration) *. (t*t))
    in 
    if Wall.isInRectangle wall (newPos deltaTime)
        then return $ ball^._referenceTime + realToFrac deltaTime
        else Nothing


--get list of possible intersection-times
intersectionHelper :: Vec3 Float -> Vec3 Float -> Ball -> [Float]
intersectionHelper wallNormal wallCenter ball = 
    let 
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
        a  = (ball^._acceleration)        `dot` wallNormal
        b  = (ball^._speed)               `dot` wallNormal
        c1 = (relativePos +. ballOffset) `dot` wallNormal
        c2 = (relativePos -. ballOffset) `dot` wallNormal
    in 
    concat $ map (\(x,y) -> [x,y]) $ catMaybes
         [ roots a b c1
         , roots a b c2 ]
        

-- from stack overflow
roots :: Float -> Float -> Float -> Maybe (Float, Float)
roots a b c = if d < 0 then
                Nothing
                else Just (x1, x2)
                    where x1 = e + sqrt d / (2 * a)
                          x2 = e - sqrt d / (2 * a)
                          d = b * b - 4 * a * c
                          e = - b / (2 * a)
