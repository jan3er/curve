{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Player where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.List 

import qualified Curve.Game.Math as M
import qualified Curve.Game.Wall as Wall
import           Curve.Game.Wall (Wall)
import qualified Curve.Game.Paddle as Paddle
import           Curve.Game.Paddle (Paddle)
import           Control.Lens

--------------------------------

data Player = Player 
    { _player_wall   :: Wall
    , _player_paddle :: Paddle
    } deriving Show
makeLenses ''Player

type PlayerMap = Map Int Player

--------------------------------

new :: Player
new = Player 
    (Wall.init (M.mkVec3 0 0 0) (M.mkVec3 0 0 0) (M.mkVec3 0 0 0)) 
    (Paddle.Paddle [])


add :: Player -> PlayerMap -> (PlayerMap, Int)
add player pm =
    let nr = fromJust $ find (\x -> x `notElem` map fst (Map.toList pm)) [0..]
    in (Map.insert nr player pm, nr)


{-fromClientMap:: Map Int a -> PlayerMap-}
{-fromClientMap = Map.fromList . (map $ \(nr, _) -> (nr, new)) . Map.toList-}

