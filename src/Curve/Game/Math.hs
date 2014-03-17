{-# OPTIONS -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Curve.Game.Math 
    ( module Data.Vec
    , (+.)
    , (-.)
    , (*.)
    , mkTuple3
    , mkTuple4
    , mkVec3
    , mkVec4
    , mkVec3Uncurry
    , mkVec4Uncurry
    , from33to44
    ) where

{-import           Control.Applicative-}
import           Data.Vec
import qualified Data.Vec as V

import Data.Aeson
import Control.Monad

--------------------------

instance ToJSON (Vec3 Float) where
   toJSON (x:.y:.z:.())= object ["x" .= x, "y" .= y, "z" .= z]

instance FromJSON (Vec3 Float) where
    parseJSON (Object v) = do
        x <- v .: "x"
        y <- v .: "y"
        z <- v .: "z"
        return $ (x:.y:.z:.())
    parseJSON _          = mzero
        

-- A non-Object value is of the wrong type, so use mzero to fail.
--------------------------

mkTuple3 :: Vec3 a -> (a,a,a)
mkTuple3 (x:.y:.z:.()) = (x,y,z)

mkTuple4 :: Vec4 a -> (a,a,a,a)
mkTuple4 (w:.x:.y:.z:.()) = (w,x,y,z)

mkVec3 :: a -> a -> a -> Vec3 a
mkVec3 x y z = x V.:. y V.:. z V.:. ()

mkVec4 :: a -> a -> a -> a -> Vec4 a
mkVec4 w x y z = w V.:. x V.:. y V.:. z V.:. ()

mkVec3Uncurry :: (a,a,a) -> Vec3 a
mkVec3Uncurry (x,y,z) = x V.:. y V.:. z V.:. ()

mkVec4Uncurry :: (a,a,a,a) -> Vec4 a
mkVec4Uncurry (w,x,y,z) = w V.:. x V.:. y V.:. z V.:. ()

(-.) :: V.Vec3 Float -> V.Vec3 Float -> V.Vec3 Float
(-.) a b = V.zipWith (-) a b

(+.) :: V.Vec3 Float -> V.Vec3 Float -> V.Vec3 Float
(+.) a b = V.zipWith (+) a b

(*.) :: V.Vec3 Float -> Float -> V.Vec3 Float
(*.) v a = V.map (a*) v

from33to44 :: Mat33 Float -> Mat44 Float
from33to44 v3 = (V.map (\v -> v `append` 0) v3) `append` (mkVec4 0 0 0 1 V.:. ())
