{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.Types where 

import           Data.Time
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Data.List
import           Data.Maybe
import           Network.Socket

import           Curve.Network.Types
import           Curve.Game.Types

import           Control.Lens
import           Control.Monad
{-import           Control.Concurrent-}
import           Control.Applicative


----------------------------------------

-- represends one client
data SClient = SClient 
    { _scl_socket   :: Socket
    , _scl_client   :: Client
    } deriving Show
makeLenses ''SClient

type PlayerMap = Map.Map Int (Player, Maybe SClient)

-- holds the enviornments state
data Env = Env 
    { _env_playerMap   :: PlayerMap
    , _env_isRunning   :: Bool
    , _env_startTime   :: UTCTime
    , _env_currentTime :: NominalDiffTime
    } deriving Show
makeLenses ''Env

-----------------------------------------
-- OPERATIONS ON SCLIENT

-----------------------------------------
--OPERATIONS ON PLAYERMAP

-- get client or throw error if it does not exist
clientFromNr :: Int -> PlayerMap -> SClient
clientFromNr nr pm = 
    let entry = Map.lookup nr pm
        maybeClient = join (snd <$> entry)
    in maybe (error "Server.Types.PlayerMap.socketFromNr") id maybeClient

-- return nrs of all connected clients
connectedClientsNr :: PlayerMap -> [Int]
connectedClientsNr =
    let isAlive  = maybe False (view $ scl_client.cl_isAlive) . view _2
    in Set.toList . Map.keysSet . Map.filter isAlive

-- add a new client-player-pair to the pm and return nr of new entry
addClient :: SClient -> PlayerMap -> (PlayerMap, Int)
addClient client pm =
  let player = Player []
      nr = fromJust $ find (\x -> x `notElem` map fst (Map.toList pm)) [0..]
  in (Map.insert nr (player, Just client) pm, nr)


-- remove client from player map if game is not running jet
-- otherwise mark client with this nr as dead
removeOrKillClient :: Bool -> Int -> PlayerMap -> PlayerMap
removeOrKillClient isRunning nr pm = 
    let f = if isRunning then kill else remove
    in 
    Map.alter (maybe (error "Server.Types.PlayerMap.removeOrKillClient") f) nr pm
  where 
    remove _    = Nothing
    kill (p, c) = Just(p, set (scl_client.cl_isAlive) False <$> c )
    

-----------------------------------------
-- OPERATIONS ON ENV






-----------------------------------------

            
-- add a new client-player-pair to the pm, returns nr of new entry
{-addClient' :: PlayerMap -> Socket -> String -> UTCTime -> (PlayerMap, Int)-}
{-addClient' pm sock nick time =-}
  {-let client = SClient { _scl_socket   = sock,-}
                         {-_scl_client   = Client { _cl_nick    = nick,-}
                                                  {-_cl_lastMsg = 0,-}
                                                  {-_cl_isAlive = True -}
                                                {-}-}
                       {-}-}
      {-player = Player { _player_posList = [] }-}
      {-nr = (\(Just x) -> x) $ find (\x -> x `notElem` map fst (Map.toList pm)) [0..]-}
  {-in (Map.insert nr (player, Just client) pm, nr)-}


