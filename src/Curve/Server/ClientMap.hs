{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.ClientMap where 

import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
{-import           Data.List-}
{-import           Data.Maybe-}
import           Network.Socket

import           Curve.Network.Types
{-import           Curve.Game.Player-}

import           Control.Lens
{-import           Control.Monad-}
{-import           Control.Applicative-}

----------------------------------------

-- represends one client
data SClient = SClient 
    { _scl_socket   :: Socket
    , _scl_client   :: Client
    } deriving Show
makeLenses ''SClient

type ClientMap = Map Int SClient

----------------------------------------

-- get client or throw error if it does not exist
clientFromNr :: Int -> ClientMap -> SClient
clientFromNr nr cm = 
     maybe (error "Server.Types.PlayerMap.socketFromNr") id $ Map.lookup nr cm

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
{-let player = Player []-}
    {-nr = fromJust $ find (\x -> x `notElem` map fst (Map.toList pm)) [0..]-}
{-in (Map.insert nr (player, Just client) pm, nr)-}


-- remove client from map if game is not running jet
-- otherwise mark client with this nr as dead
removeOrKillClient :: Bool -> Int -> ClientMap -> ClientMap
removeOrKillClient isRunning nr cm = 
    let f = if isRunning then kill else remove
    in 
    Map.alter (maybe (error "Server.PlayerMap.removeOrKillClient") f) nr cm
  where 
    remove _ = Nothing
    kill c   = Just $ set (scl_client.cl_isAlive) False  c
    
