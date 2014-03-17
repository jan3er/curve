{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Curve.Game.Message where

import System.IO
import Data.Time
import Control.Concurrent
import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.Loops
import Control.Exception hiding (handle)
import Data.Aeson.TH

import Curve.Game.Network
import Curve.Game.Utils
import Curve.Game.Ball


-------------------------------------


-- a consists of three parts and is called for each received message
--
-- MessageHandlerPre and MessageHandlerPost perform io-actions or update the local state
-- before and after receiving a message
--
-- MessageHandlerPure handles messages purely. It may update the local state
-- and return a list of reply messages and their receivers

type MessageHandlerPre  a = StateT a IO ()
type MessageHandlerPost a = StateT a IO ()
type MessageHandlerPure a = Handle -> Message -> State a [(Handle, Message)]
data MessageHandler     a = MessageHandler 
                            (MessageHandlerPre a)
                            (MessageHandlerPure a)
                            (MessageHandlerPost a)

runHandler :: Handle -> Message -> MessageHandler a -> StateT a IO [(Handle, Message)]
runHandler handle msg (MessageHandler preHandler pureHandler postHandler) = do
    preHandler
    msgs <- liftState (pureHandler handle msg)
    postHandler
    return msgs
    

---------------------------------------------


-- Message is the central datatype for network communication
-- Messages of this type are serialized and sent back and forth.
-- communication happens between the server and each client,
-- but not between clients.
--
-- messages prefixed with a C are intended to be send from client to server
-- and messages prefixed with a S from server to client
-- messages without a prefix may be sent in both directions

data Message = 
        
    -- sent to server when establishing the connection
      CMessageHello   { _CMessageHello_nick      :: String }
    
    -- broadcasted everytime a player connects/disconnects etc.
    | SMessageClients { _SMessageClients_clients   :: [Client] -- all clients in the game
                      , _SMessageClients_index     :: Int }    -- receivers client is at 
                                                               -- this index of the list

    -- broadcasts the state of the ball everytime it bounces of a wall 
    | SMessageBall    { _SMessageBall_ball :: Ball }
    ---------

    -- broadcasted everytime a player connects/disconnects etc.
    {-| SMessageWorld     { _SMessageWorld_clients   :: [(Int, Maybe Client)]-}
                    {-, _SMessageWorld_clientNr  :: Int-}
                    {-, _SMessageWorld_isRunning :: Bool }-}
    

    -- broadcasts the state of the ball everytime it bounces of a wall 
    {-| SMessageBall    { _SMessageBall_referenceTime :: NominalDiffTime-}
                      {-, _SMessageBall_position      :: (Float, Float, Float)-}
                      {-, _SMessageBall_direction     :: (Float, Float, Float)-}
                      {-, _SMessageBall_acceleration  :: (Float, Float, Float)-}
                      {-, _SMessageBall_speed         :: Float-}
                      {-, _SMessageBall_size          :: Float }-}

    -- sent by client and server distribute each paddle's 
    -- current position through the network
    | MessagePaddle   { _MessagePaddle_nr        :: Int
                      , _MessagePaddle_pos       :: (NominalDiffTime, Float, Float) }

    -- synchronize clients to the server's clock
    | MessageTime     { _MessageTime_time        :: NominalDiffTime }

    {-| MessageUnknown -}
makeLenses ''Message
deriveJSON defaultOptions ''Message


-----------------------------------


-- receive messages on an established connection and call the
-- handler on them
-- TODO: maybe wrap into ErrorT monad
getMessageAndHandle :: Show a => MVar a -> Handle -> MessageHandler a -> IO ()
getMessageAndHandle mEnv handle handler = do
    result :: Either IOException () <-
        try $ whileM_ (not <$> hIsEOF handle) $ do
            maybeMessage <- getMessage handle
            case maybeMessage of
                Just msg -> do
                    modifyMVarStateT mEnv $ do
                        msgs <- runHandler handle msg handler
                        liftIO $ putMessages msgs
                Nothing -> do
                    putStrLn "WARNING: could not decrypt msg"
    case result of
        Left _   -> return ()
        Right () -> return ()
