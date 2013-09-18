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
getMsgAndHandle mEnv handle handler =
    
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
    whileM_ (not <$> hIsEOF handle) (maybe handleNoMsg handleMsg =<< getMsg handle)

    -- todo: close connection?
    return ()

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
