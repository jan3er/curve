{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Network.Types where

import           Data.Time
import           Data.Typeable
import           Data.Data

import           Curve.Game.Types


-- represent messages
data Message = TimeMessage { mTIME :: UTCTime }
             | WorldMessage { mWORLD :: Int }
             | UnknownMessage deriving (Data, Typeable, Show)
