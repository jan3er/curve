{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Network.Types where

import           Data.Time
import           Data.Typeable
import           Data.Data
import           Control.Lens
import           Control.Monad.State
import           Network.Socket

type NetworkTime = NominalDiffTime

data Client = Client {
  _cl_nick     :: String,
  _cl_lastMsg  :: NetworkTime,
  _cl_isAlive  :: Bool
} deriving (Data, Typeable, Show)
makeLenses ''Client


data Msg = CMsgHello     { _CMsgHello_nick      :: String }
         | SMsgWorld     { _SMsgWorld_clients   :: [(Int, Maybe Client)]
                         , _SMsgWorld_clientNr  :: Int
                         , _SMsgWorld_isRunning :: Bool }
         | MsgPaddle     { _MsgPaddle_nr        :: Int
                         , _MsgPaddle_pos       :: (NetworkTime, Float, Float) }
         | MsgTime       { _MsgTime_time        :: NominalDiffTime }
         | MsgUnknown 
  deriving (Data, Typeable, Show)
makeLenses ''Msg


type MsgHandlerPure a = Msg -> State a [(Socket, Msg)]
type MsgHandlerPre  a = StateT a IO ()
type MsgHandlerPost a = StateT a IO ()
type MsgHandler     a = (MsgHandlerPre a, MsgHandlerPure a, MsgHandlerPost a)
