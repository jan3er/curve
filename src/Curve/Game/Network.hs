{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Curve.Game.Network where

import System.IO
import Data.Time
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BLC

---------------------------------------------

deriveJSON defaultOptions ''NominalDiffTime

-- represents a single client
-- this datastructure is used by client and server alike
data Client = Client {
  _cl_nick         :: String,
  _cl_lastMessage  :: NominalDiffTime,
  _cl_isAlive      :: Bool,
  _cl_playerId     :: Int
} deriving (Show, Eq)
makeLenses ''Client
deriveJSON defaultOptions ''Client


-------------------------------------

-- send messages
putMessages :: ToJSON m => [(Handle, m)] -> IO ()
putMessages = mapM_ (\(h,m) -> putMessage h m)

putMessage :: ToJSON m => Handle -> m-> IO ()
putMessage handle = hPutStrLn handle . BLC.unpack . encode

-- receive messages
getMessage :: FromJSON m => Handle -> IO (Maybe m)
getMessage handle = hGetLine handle >>= return . decode . BLC.pack
