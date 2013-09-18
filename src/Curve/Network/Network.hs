{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Curve.Network.Network 
    ( module Curve.Network.Types
    , putMsg
    , putMsgs
    , getMsg
    , getMsgAndHandle
    ) where

import           System.IO
import           Control.Concurrent
import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Loops
import           Data.Aeson.Generic
import qualified Data.ByteString.Lazy.Char8 as BLC

import           Curve.Network.Types        

-------------------------------------

-- runs until connection is closed
-- handle messages with given handler
getMsgAndHandle :: Show a => MVar a -> Handle -> MsgHandler a -> IO ()
getMsgAndHandle mEnv hdl handler =
    
    -- process a message by calling the handler on it
    let handleMsg msg = do
            modifyMVar_ mEnv $ execStateT $ do
                handler^._1
                msgs <- StateT (return . runState ((handler^._2) msg))
                handler^._3
                liftIO $ mapM_ (\(h,m) -> putMsg h m) msgs

        handleNoMsg = do
            putStrLn "WARNING: could not decrypt msg"
    in do
    -- process messages as long as the connection is up
    whileM_ (not <$> hIsEOF hdl) (maybe handleNoMsg handleMsg =<< getMsg hdl)

    -- todo: close connection?
    return ()

putMsgs :: [(Handle, Msg)] -> IO ()
putMsgs = mapM_ (\(h,m) -> putMsg h m)

putMsg :: Handle -> Msg -> IO ()
putMsg hdl msg = do
    putStrLn $ "=> outgoing: " ++ show msg
    putMsg' hdl msg

getMsg :: Handle -> IO (Maybe Msg)
getMsg hdl = do
    msg <- getMsg' hdl
    putStrLn $ "=> incomming: " ++ show msg
    return msg

    
putMsg' :: Handle -> Msg -> IO ()
putMsg' hdl = hPutStrLn hdl . BLC.unpack . encode

getMsg' :: Handle -> IO (Maybe Msg)
getMsg' hdl = hGetLine hdl >>= return . decode . BLC.pack
