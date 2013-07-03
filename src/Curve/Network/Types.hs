{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Network.Types where

import           Data.Time
import           Data.Typeable
import           Data.Data

import           Control.Category
import           Data.Label
import           Prelude hiding ((.), id)

import           Curve.Game.Types

data Client = Client {
  _cl_nick     :: String,
  _cl_lastMsg  :: UTCTime,
  _cl_isAlive  :: Bool
} deriving (Data, Typeable, Show)
$(mkLabels [''Client])

-- represent messages
data Msg = CMsgHello { _msg_nick    :: String }
         | SMsgWorld { _msg_clients :: [(Int, Maybe Client)] }
         |  MsgUnknown deriving (Data, Typeable, Show)
$(mkLabels [''Msg])

             {-| TimeMessage { mTIME :: UTCTime }-}
