{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Game.Wall where

import           Prelude hiding (init)
{-import           Data.Maybe-}
{-import           Data.List hiding (init)-}
{-import           Data.Time-}
{-import           Debug.Trace-}
import           Control.Lens
import           Control.Applicative

import qualified Curve.Game.Math as M
import           Curve.Game.Math (Vec3, Mat33)

-----------------------------------

data Wall = Wall
    { _lowerLeft     :: Vec3 Float
    , _lowerRight    :: Vec3 Float
    , _upperLeft     :: Vec3 Float
    , _upperRight    :: Vec3 Float
    } deriving Show
makeLenses ''Wall

-----------------------------------

-- this is probably horribly wrong
-- TODO add top and bottom
initArena :: Float -> Float -> Int -> ([Wall],[Wall])
initArena radius height nrs = 
    let angle  :: Float = 2*pi / (fromIntegral nrs)
        top    :: Float = height/2
        bottom :: Float = (-1)*height/2
        stepToPos :: Float -> Int -> Vec3 Float
        stepToPos h i =  M.mkVec3 (sin (fromIntegral i*angle) * radius)
                                  (cos (fromIntegral i*angle) * radius) h
    in
    ((\i -> 
        init
        (stepToPos bottom (i))
        (stepToPos bottom (i+1))
        (stepToPos top    (i))
    ) <$> [1..nrs]
    ,
    (\h -> 
        init
        (M.mkVec3 (-radius) ( radius) h)
        (M.mkVec3 (-radius) ( radius) h)
        (M.mkVec3 (-radius) ( radius) h)
    ) <$> [top, bottom])
    


-----------------------------------
-- TODO
-- change this to a rectangle with following values:
--      normal
--      updir
--      center
--      (width, height)


---------------------------------

-- positions as facing the center of the world
init :: Vec3 Float -> Vec3 Float -> Vec3 Float -> Wall
init lowerLeft' lowerRight' upperLeft' =
    let upperRight' = lowerRight' M.-. lowerLeft' M.+. upperLeft'
    in Wall lowerLeft' lowerRight' upperLeft' upperRight'


-- the normal of the plane, leaning towards (0,0,0)
normal :: Wall -> Vec3 Float
normal wall = 
    let p1 = (wall^.upperLeft)  M.-. (wall^.lowerLeft)
        p2 = (wall^.lowerRight) M.-. (wall^.lowerLeft)
    in M.normalize $ M.cross p1 p2


-- normalized vector pointing up
up :: Wall -> Vec3 Float
up wall = M.normalize $ (wall^.upperLeft)  M.-. (wall^.lowerLeft)

center :: Wall -> Vec3 Float
center wall = ((wall^.upperLeft) M.+. (wall^.lowerRight)) M.*. 0.5

{-size :: Wall -> (Float, Float) -}
{-size wall = (-}


-- the minimal signed distance of the plane to zero
distance :: Wall -> Float
distance wall =
    -1 * (normal wall) `M.dot` (wall^.upperLeft)


-- used by isInRectangle
-- these are the columns of a linear transformation defined by
-- (lowerRight-lowerLeft           -> 1,0,0)
-- (upperLeft-lowerLeft            -> 0,1,0)
-- (something orthogonal to above  -> 0,0,1)
transformation :: Wall -> Mat33 Float
transformation wall =
    let v1 = (wall^.lowerRight) M.-. (wall^.lowerLeft)
        v2 = (wall^.upperRight) M.-. (wall^.lowerLeft)
        v3 = v1 `M.cross` v2
        m  = M.mkVec3 v1 v2 v3
    in 
    -- TODO maybe it has to be transposed
    maybe (error "Curve.Game.Wall.transformation is singular") id $ M.invert m


isInRectangle :: Wall -> Vec3 Float -> Bool
isInRectangle wall ipAbs =
    let ip  = ipAbs M.-. (wall^.lowerLeft)
        res = (transformation wall) `M.multmv` ip
    in M.fold (&&) $ M.map (\x -> 0 <= x && x <= 1) res




