{-# OPTIONS -Wall #-}

module Curve.Game.Math 
    ( module Data.Vec
    , (+.)
    , (-.)
    , (*.)
    , mkVec3
    ) where

import Data.Vec
import qualified Data.Vec as V

--------------------------

mkVec3 :: a -> a -> a -> Vec3 a
mkVec3 x y z = x V.:. y V.:. z V.:. ()

(-.) :: V.Vec3 Float -> V.Vec3 Float -> V.Vec3 Float
(-.) a b = V.zipWith (-) a b

(+.) :: V.Vec3 Float -> V.Vec3 Float -> V.Vec3 Float
(+.) a b = V.zipWith (+) a b

(*.) :: V.Vec3 Float -> Float -> V.Vec3 Float
(*.) v a = V.map (a*) v
