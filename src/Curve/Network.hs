{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Network where

import           Data.Time
import           Data.Aeson.Generic
import           Data.Typeable
import           Data.Data

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL

import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString

-- represent messages
data Message = TimeMessage { mTIME :: UTCTime }
             | UnknownMessage deriving (Data, Typeable, Show)


-- receive a message over socket
recvMessage :: Socket -> IO (Maybe Message)
recvMessage sock = do
  line <- recv sock 10000
  if B.null line
    then return Nothing
    else return $ Just (decodeMessage line)
  where
    decodeMessage line = 
      case decode (BL.fromChunks [line]) :: Maybe Message of
        Nothing -> UnknownMessage
        Just x  -> x


-- send a message over socket
sendMessage :: Socket -> Message -> IO ()
sendMessage sock message = do
  let line = encode message :: BL.ByteString
  _ <- send sock (B.concat $ BL.toChunks line)
  return ()

