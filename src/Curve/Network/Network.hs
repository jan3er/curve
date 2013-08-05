{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Network.Network (
  module Curve.Network.Types,
  sendMsg,
  recvMsg
  ) where

import           Data.Aeson.Generic

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL

import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString

import           Curve.Network.Types        


-- TODO make creators for server/client sockets

-- receive a message over socket
-- returns nothing if connection is dead
-- returns Just Message otherwise
recvMsg :: Socket -> IO (Maybe Msg)
recvMsg sock = do
  line <- recv sock 10000
  if B.null line
    then return Nothing
    else return $ Just (decodeMsg line)
  where
    decodeMsg line = 
      case decode (BL.fromChunks [line]) :: Maybe Msg of
        Nothing -> MsgUnknown
        Just x  -> x


-- send a msg over socket
sendMsg :: Msg -> Socket -> IO ()
sendMsg msg sock = do
  let line = encode msg :: BL.ByteString
  _ <- send sock (B.concat $ BL.toChunks line)
  return ()

