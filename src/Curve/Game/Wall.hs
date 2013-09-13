{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Game.Wall where

import           Data.Maybe
import           Data.List
import           Data.Time
import           Data.Vec ( (:.) )
import qualified Data.Vec as V
import           Debug.Trace
import           Control.Lens
import           Control.Applicative

import           Curve.Game.Ball

-----------------------------------

data Wall = Wall
    { _wall_maybeNr       :: Maybe Int
    , _wall_lowerLeft     :: V.Vec3 Float
    , _wall_lowerRight    :: V.Vec3 Float
    , _wall_upperLeft     :: V.Vec3 Float
    , _wall_upperRight    :: V.Vec3 Float
    } deriving Show
makeLenses ''Wall

-----------------------------------

minus a b = V.zipWith (-) a b
plus a b  = V.zipWith (-) a b

scale :: V.Vec3 Float -> Float -> V.Vec3 Float
scale v a = V.map (a*) v
{-scale a v = V.map (a*) v-}

-----------------------------------

-- this is probably horribly wrong
-- TODO add top and bottom
initArena :: [Int] -> [Wall]
initArena nrs = 
    let angle  :: Float = 2*pi / (fromIntegral $ length nrs)
        radius :: Float = 10
        height :: Float = 4
        stepToPos h i =  (sin (fromIntegral i*angle) * radius) V.:. 
                         (cos (fromIntegral i*angle) * radius) V.:. 
                         h V.:. ()
    in
    (\(nr, i) -> 
        initWall
        (Just nr)
        (stepToPos ((-1)*height/2) (i))
        (stepToPos ((-1)*height/2) (i+1))
        (stepToPos (( 1)*height/2) (i))
    ) <$> zip nrs [1 .. (length nrs)]
    


-----------------------------------

-- positions as facing the center of the world
initWall :: Maybe Int -> V.Vec3 Float -> V.Vec3 Float -> V.Vec3 Float -> Wall
initWall maybeNr lowerLeft lowerRight upperLeft =
    let upperRight = lowerRight `minus` lowerLeft `plus` upperLeft
    in Wall maybeNr lowerLeft lowerRight upperLeft upperRight


-- the normal of the plane, pointing towards (0,0,0)
normal :: Wall -> V.Vec3 Float
normal wall = 
    let p1 = (wall^.wall_upperLeft)  `minus` (wall^.wall_lowerLeft)
        p2 = (wall^.wall_lowerRight) `minus` (wall^.wall_lowerLeft)
    in V.normalize $ V.cross p1 p2

-- the minimal signed distance of the plane to zero
distance :: Wall -> Float
distance wall =
    -1 * (normal wall) `V.dot` (wall^.wall_upperLeft)


-- these are the columns of a linear transformation defined by
-- (lowerRight-lowerLeft           -> 1,0,0)
-- (upperLeft-lowerLeft            -> 0,1,0)
-- (something orthogonal to above  -> 0,0,1)
transformation :: Wall -> V.Mat33 Float
transformation wall =
    let v1 = (wall^.wall_lowerRight) `minus` (wall^.wall_lowerLeft)
        v2 = (wall^.wall_upperRight) `minus` (wall^.wall_lowerLeft)
        v3 = v1 `V.cross` v2
        m  = v1 V.:. v2 V.:. v3 V.:. ()
    in 
    -- TODO maybe it has to be transposed
    maybe (error "Curve.Game.Wall.transformation is singular") id $ V.invert m


isInRectangle :: Wall -> V.Vec3 Float -> Bool
isInRectangle wall ipAbs =
    let ip  = ipAbs `minus` (wall^.wall_lowerLeft)
        res = (transformation wall) `V.multmv` ip
    in V.fold (&&) $ V.map (\x -> 0 <= x && x <= 1) res




intersectionList :: Ball -> [Wall] -> Wall
intersectionList ball walls =
    let f wall = (\time -> (wall, time)) <$> intersection ball wall
        tuples :: [(Wall, NominalDiffTime)] = catMaybes $ f <$> walls
    in
    case tuples of
        [] -> error "Curve.Game.Wall intersectionList: the ball touched no wall!"
        xs -> fst $ maximumBy (\a b -> compare (a^._2) (b^._2)) xs
    


intersection :: Ball -> Wall -> Maybe NominalDiffTime
intersection ball wall =
    let maybeTime = intersection' ball wall
        newPos t  = (ball^.ball_position)
                    `plus` ((ball^.ball_speed)        `scale` t)
                    `plus` ((ball^.ball_acceleration) `scale` (t*t))
        
    in do
        t <- maybeTime
        if isInRectangle wall (newPos t)
            then return $ realToFrac t
            else Nothing
    

-- pretty fucked up. 
-- repeat basic calculations from scrath
intersection' :: Ball -> Wall -> Maybe Float
intersection' ball wall =
    let isSmall x = abs x < 0.000001
        pos       = ball^.ball_position
        speed     = ball^.ball_speed
        accel     = ball^.ball_acceleration
        ballSize  = ball^.ball_size
    in
    if isSmall $ normal wall `V.dot` accel
    then 
        if isSmall $ normal wall `V.dot` speed
        then
             Nothing
        else 
            Just $ ((normal wall `V.dot` pos) + (distance wall - ballSize)) / normal wall `V.dot` speed
    else 
        let p   = (normal wall `V.dot` speed) / (normal wall `V.dot` accel)
            q   = (normal wall `V.dot` pos) + (distance wall - ballSize) / (normal wall `V.dot` accel)
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
