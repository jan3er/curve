{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Network.Types where

import           Data.Time
import           Data.Typeable
import           Data.Data
import           Control.Lens

import           Curve.Game.Types


data Client = Client {
  _cl_nick     :: String,
  _cl_lastMsg  :: UTCTime,
  _cl_isAlive  :: Bool
} deriving (Data, Typeable, Show)
makeLenses ''Client

-- represent messages
data Msg = CMsgHello     { _CMsgHello_nick      :: String }
         | SMsgWorld     { _SMsgWorld_clients   :: [(Int, Maybe Client)]
                         , _SMsgWorld_clientNr  :: Int
                         , _SMsgWorld_isRunning :: Bool }
         | MsgPaddle     { _MsgPaddle_nr        :: Int
                         , _MsgPaddle_pos       :: (UTCTime, Float, Float) }
         | MsgTime       { _MsgTime_time        :: UTCTime }
         | MsgUnknown 
  deriving (Data, Typeable, Show)
makeLenses ''Msg

             {-| TimeMessage { mTIME :: UTCTime }-}
