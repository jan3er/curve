{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Curve.Network.Network where

import System.IO
import Data.Time
import Data.Typeable
import Data.Data
import Control.Concurrent
import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.Loops
import Data.Aeson.Generic
import qualified Data.ByteString.Lazy.Char8 as BLC

---------------------------------------------

type NetworkTime = NominalDiffTime



-- represents a single client
-- this datastructure is used by client and server alike
data Client = Client {
  _cl_nick     :: String,
  _cl_lastMsg  :: NetworkTime,
  _cl_isAlive  :: Bool
} deriving (Data, Typeable, Show)
makeLenses ''Client



-- Msg is the central datatype for network communication
-- Messages of this type are serialized and sent back and forth.
-- communication happens between the server and each client,
-- but not between clients.
--
-- messages prefixed with a C are intended to be send from client to server
-- and messages prefixed with a S from server to client
-- messages without a prefix may be sent in both directions

data Msg = 
        
    -- sent to server when establishing the connection
      CMsgHello     { _CMsgHello_nick      :: String }

    -- broadcasted everytime a player connects/disconnects etc.
    | SMsgWorld     { _SMsgWorld_clients   :: [(Int, Maybe Client)]
                    , _SMsgWorld_clientNr  :: Int
                    , _SMsgWorld_isRunning :: Bool }

    -- broadcasts the state of the ball everytime it bounces of a wall 
    | SMsgBall      { _SMsgBall_time       :: NetworkTime
                    , _SMsgBall_position   :: (Float, Float, Float)
                    , _SMsgBall_direction  :: (Float, Float, Float)
                    , _SMsgBall_spin       :: (Float, Float, Float) }

    -- sent by client and server distribute each paddle's 
    -- current position through the network
    | MsgPaddle     { _MsgPaddle_nr        :: Int
                    , _MsgPaddle_pos       :: (NetworkTime, Float, Float) }

    -- synchronize clients to the server's clock
    | MsgTime       { _MsgTime_time        :: NetworkTime }

    {-| MsgUnknown -}
    deriving (Data, Typeable, Show)
makeLenses ''Msg



-- a message handler is called for each received message
-- it consists of three parts
--
-- MsgHandlerPre and MsgHandlerPost perform io-actions or update the local state
-- before and after receiving a message
--
-- MsgHandlerPure handles messages purely. It may update the local state
-- and return a list of reply messages and their receivers

type MsgHandlerPre  a = StateT a IO ()
type MsgHandlerPost a = StateT a IO ()
type MsgHandlerPure a = Msg -> State a [(Handle, Msg)]
type MsgHandler     a = ( MsgHandlerPre a
                        , MsgHandlerPure a
                        , MsgHandlerPost a )

-------------------------------------


-- one a connection is established this method
-- is called to receive and handle all incomming messages
-- it runs until the connection is closed

getMsgAndHandle :: Show a => MVar a -> Handle -> MsgHandler a -> IO ()
getMsgAndHandle mEnv handle handler =
    
    -- process a message by calling the handler on it
    let handleMsg msg = do
            modifyMVar_ mEnv $ execStateT $ do
                -- MsgHandlerPre
                handler^._1
                -- MsgHandlerPure
                msgs <- StateT (return . runState ((handler^._2) msg))
                -- MsgHandlerPost
                handler^._3
                -- send generated messages
                liftIO $ mapM_ (\(h,m) -> putMsg h m) msgs

        handleNoMsg = do
            putStrLn "WARNING: could not decrypt msg"
    in do
    -- process messages as long as the connection is up
    whileM_ (not <$> hIsEOF handle) (maybe handleNoMsg handleMsg =<< getMsg handle)



putMsgs :: [(Handle, Msg)] -> IO ()
putMsgs = mapM_ (\(h,m) -> putMsg h m)

putMsg :: Handle -> Msg -> IO ()
putMsg handle msg = do
    putStrLn $ "=> outgoing: " ++ show msg
    putMsg' handle msg

getMsg :: Handle -> IO (Maybe Msg)
getMsg handle = do
    msg <- getMsg' handle
    putStrLn $ "=> incomming: " ++ show msg
    return msg

    
putMsg' :: Handle -> Msg -> IO ()
putMsg' handle = hPutStrLn handle . BLC.unpack . encode

getMsg' :: Handle -> IO (Maybe Msg)
getMsg' handle = hGetLine handle >>= return . decode . BLC.pack
