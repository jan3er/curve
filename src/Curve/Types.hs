{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}

module Curve.Types where

import Data.Time
import Data.Typeable
import Data.Data
import Network.Socket

-- will hold the enviornment state
data Env = Env {
   envClients   :: [Client]
  ,envAcceptNew :: Bool
} deriving Show

-- represends one client
data Client = Client {
      clientId       :: Int
     ,clientSocket   :: Socket
     ,clientNick     :: String
     ,clientLastPong :: UTCTime
    } deriving Show
instance Eq Client where
  a == b = (clientId a) == (clientId b)

-- represents a message 
data Message = TextMessage { mTEXT :: String } 
             | TimeMessage { mTIME :: UTCTime } deriving (Data, Typeable, Show)
