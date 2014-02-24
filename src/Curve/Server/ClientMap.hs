{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.ClientMap where 

import           Control.Lens
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
{-import           Data.List-}
import           Data.Maybe
import           System.IO
{-import           Network.Socket-}

import           Curve.Game.Network
{-import           Curve.Game.Player-}

{-import           Control.Monad-}
{-import           Control.Applicative-}

----------------------------------------

-- represends one client
data SClient = SClient 
    { _scl_handle   :: Handle
    , _scl_client   :: Client
    } deriving Show
makeLenses ''SClient

type ClientMap = Map Int SClient

----------------------------------------

-- get client or throw error if it does not exist
clientFromNr :: Int -> ClientMap -> SClient
clientFromNr nr cm = 
     fromMaybe (error "Server.Types.PlayerMap.socketFromNr") $ Map.lookup nr cm

-- return nrs of all connected clients
connectedClientsNr :: ClientMap -> [Int]
connectedClientsNr =
    let isAlive  = view $ scl_client.cl_isAlive
    in Set.toList . Map.keysSet . Map.filter isAlive

-- add a new client to map
addClient :: SClient -> Int -> ClientMap -> ClientMap
addClient client nr =
    let f x = case x of
                Just _ -> error "Server.ClientMap.addClient nr already in map"
                Nothing -> Just client
    in Map.alter f nr
    
removeClient :: Int -> ClientMap -> ClientMap
removeClient = Map.alter (maybe (error "Server.ClientMap.remove") (\_ -> Nothing))
    
killClient :: Int -> ClientMap -> ClientMap
killClient = 
    let kill = Just . set (scl_client.cl_isAlive) False
    in Map.alter (maybe (error "Server.PlayerMap.removeOrKillClient") kill)
