{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Network.Network (
    module Curve.Network.Types,
    sendMsg,
    recvMsgAndHandle,
    recvMsg
    ) where

import           Control.Concurrent
import           Control.Monad.State

import           Data.Aeson.Generic

import           Debug.Trace

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL

import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString

import           Curve.Network.Types        


-------------------------------------------------------------------------------
-- receiving ------------------------------------------------------------------
-------------------------------------------------------------------------------

recvMsgAndHandle :: Show a => MVar a -> Socket -> MsgHandler a -> IO ()
recvMsgAndHandle mEnv sock handler = do
    maybeMsg <- recvMsg sock
    {-case maybeMsg of-}
    case trace ("=> incomming: " ++ show maybeMsg) maybeMsg of
        Nothing  ->   
            --TODO check sClose somewhere
            sClose sock
            
        Just msg -> do
            modifyMVar_ mEnv $ execStateT $ do
                list <- StateT (return . runState (handler msg))
                liftIO $ sequence_ $ map (\ (s,m) -> sendMsg m s) list
                liftIO $ putStrLn $ "sending: " ++ (show list)
            {-print =<< readMVar mEnv-}
            recvMsgAndHandle mEnv sock handler


-- receive a message over socket
-- return nothing if connection is dead
-- return Just Message otherwise
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


-------------------------------------------------------------------------------
-- sending --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- send a msg over socket
sendMsg :: Msg -> Socket -> IO ()
sendMsg msg sock = do
  let msg' = trace ("=> outgoing: " ++ (show msg)) msg
  let line = encode msg' :: BL.ByteString
  _ <- send sock (B.concat $ BL.toChunks line)
  return ()

