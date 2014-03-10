{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Player where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.List 

import qualified Curve.Game.Math as M

import           Curve.Game.Wall as Wall
import           Curve.Game.Paddle as Paddle
import           Control.Lens

--------------------------------

data Player = Player 
    { __wall   :: Wall
    , __paddle :: Paddle
    } deriving Show
makeLenses ''Player

type PlayerMap = Map Int Player

--------------------------------

new :: Player
new = Player 
    (Wall (M.mkVec3 0 0 0) (M.mkVec3 0 0 0) (M.mkVec3 0 0 0) (1,1)) 
    (Paddle [])

-- TODO ensure that all ids are in range 0 to n
-- or replace map with list and index is nr

add :: Player -> PlayerMap -> (PlayerMap, Int)
add player pm =
    let nr = fromJust $ find (\x -> x `notElem` map fst (Map.toList pm)) [0..]
    in (Map.insert nr player pm, nr)

remove :: Int -> PlayerMap -> PlayerMap
remove nr pm = Map.alter (maybe (error "Game.Player.remove ") (\_ -> Nothing)) nr pm
