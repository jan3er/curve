{-# OPTIONS -Wall #-}

module Curve.Game.Math 
    ( module Data.Vec
    , (+.)
    , (-.)
    , (*.)
    , mkVec3
    , mkVec4
    , from33to44
    ) where

import           Control.Applicative
import           Data.Vec
import qualified Data.Vec as V

--------------------------

mkVec3 :: a -> a -> a -> Vec3 a
mkVec3 x y z = x V.:. y V.:. z V.:. ()

mkVec4 :: a -> a -> a -> a -> Vec4 a
mkVec4 w x y z = w V.:. x V.:. y V.:. z V.:. ()

(-.) :: V.Vec3 Float -> V.Vec3 Float -> V.Vec3 Float
(-.) a b = V.zipWith (-) a b

(+.) :: V.Vec3 Float -> V.Vec3 Float -> V.Vec3 Float
(+.) a b = V.zipWith (+) a b

(*.) :: V.Vec3 Float -> Float -> V.Vec3 Float
(*.) v a = V.map (a*) v

from33to44 :: Mat33 Float -> Mat44 Float
from33to44 v3 = (V.map (\v -> v `append` 0) v3) `append` (mkVec4 0 0 0 1 V.:. ())
