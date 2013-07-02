{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.Types where 

import           Data.Time
import qualified Data.Map as Map
import           Data.List
import           Network.Socket

import           Curve.Network.Types
import           Curve.Game.Types

import           Control.Category
import           Data.Label
import           Prelude hiding ((.), id)



----------------------------------------

-- represends one client
data SClient = SClient {
  _scl_socket   :: Socket,
  _scl_client   :: Client
} deriving Show
$(mkLabels [''SClient])

type PlayerMap = Map.Map Int (Player, Maybe SClient)

-- holds the enviornments state
data Env = Env {
  _env_playerMap   :: PlayerMap,
  _env_acceptNew   :: Bool
} deriving Show
$(mkLabels [''Env])

-----------------------------------------

-- creates a new clean env
newEnv :: Env
newEnv = Env { _env_playerMap = Map.empty,
               _env_acceptNew = True }
            
-- add a new client-player-pair to the pm, returns id of new entry
addClient :: PlayerMap -> Socket -> String -> UTCTime -> (PlayerMap, Int)
addClient pm sock nick time =
  let client = SClient { _scl_socket   = sock,
                         _scl_client   = Client { _cl_nick    = nick,
                                                  _cl_lastMsg = time,
                                                  _cl_isAlive = True 
                                                }
                       }
      player = Player { playerPosition = (0.5, 0.5) }
      id = (\(Just x) -> x) $ find (\x -> x `notElem` map fst (Map.toList pm)) [0..]
  in (Map.insert id (player, Just client) pm, id)

-- mark client with this id as dead
killClient :: PlayerMap -> Int -> PlayerMap
killClient pm id = Map.alter f id pm
  where 
    f Nothing   = Nothing
    f (Just p)  = Just (fst p, g (snd p))
    g Nothing   = Nothing
    g (Just c)  = Just $ c -- TODO
