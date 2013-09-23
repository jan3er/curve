{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Game.Wall where

import           Prelude hiding (init)
import           Data.Maybe
{-import           Data.List hiding (init)-}
{-import           Data.Time-}
{-import           Debug.Trace-}
import           Control.Lens
import           Control.Applicative

import           Curve.Game.Math as M

-----------------------------------

-- a rectangular wall in 3d space
data Wall = Wall
    -- normalized, pointing towards the game
    { __normal        :: Vec3 Float
    -- normalized, pointing upwards, relative to wall
    , __updir         :: Vec3 Float
    -- the render or the wall
    , __center        :: Vec3 Float
    -- (width, height)
    , __dimensions    :: (Float, Float)
    } deriving Show
makeLenses ''Wall

-----------------------------------

-- first: player walls
-- second: non-player walls
initArena :: Float -> Float -> Int -> ([Wall],[Wall])
initArena radius height noPlayers =
    let 
        -- the angle change with each step
        angle  :: Float = 2*pi / (fromIntegral noPlayers)
        -- tan alpha/2 = width/radius
        width  :: Float = radius * (tan (angle/2))
        -- the center of the wall at step i
        centerAt i  =  
            M.mkVec3 
                (sin (fromIntegral i*angle) * radius)
                (cos (fromIntegral i*angle) * radius) 
                0
    in
    -- one wall for each player
    ((\i ->
        Wall
            (M.normalize $ centerAt i M.*. (-1))
            (M.normalize $ M.mkVec3 0 0 1)
            (centerAt i)
            (width, height)
    ) <$> [1..noPlayers]
    ,        
    -- top and bottom wall
    [ Wall (M.mkVec3 0 0 1) (M.mkVec3 (-1) 0 0) (M.mkVec3 0 0 ( height/2)) (radius, radius)
    , Wall (M.mkVec3 0 0 1) (M.mkVec3    1 0 0) (M.mkVec3 0 0 (-height/2)) (radius, radius)
    ])
    


-- returns true iff the orthogonal projection of ip into the wall's plane is within the wall's dimensions
isInRectangle :: Wall -> Vec3 Float -> Bool
isInRectangle wall ip =
    let matrix                 = fromJust $ M.invert $ M.mkVec3 (wall^._normal) ((wall^._normal) `M.cross` (wall^._updir)) (wall^._updir) 
        (_:.width:.height:.()) = matrix `M.multmv` (ip -. (wall^._center))
        (maxWidth, maxHeight)  = wall^._dimensions
    in (abs width < maxWidth) && (abs height < maxHeight)

