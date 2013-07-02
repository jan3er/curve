{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Server.Server where

import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Data.Time
import           Data.List
import           Network.Socket
import           Text.Show.Pretty

import           Control.Category
import           Data.Label
import           Prelude hiding ((.), id)

import           Curve.Server.Types
import           Curve.Server.Misc
import           Curve.Network.Network

{-import           Control.Monad.Loops-}
{-import           Control.Exception-}
{-import           Control.Applicative-}
{-import           Data.Functor()-}

-- handles input from keyboard
inputHandler :: MVar Env -> IO ()
inputHandler envar = forever $ do
  line <- getLine
  case line of
    "toggle"  -> putStrLn "toggling acceptNew" >> toggleAcceptNew
    "env"   -> readMVar envar >>= putStrLn . show
    _         -> putStrLn "unknown"
  where 
    toggleAcceptNew  = do
    env <- takeMVar envar
    {-putMVar envar x { envAcceptNew = not $ envAcceptNew x }-}
    putMVar envar $ set env_acceptNew False env



-- the server entry point
start :: IO ()
start = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  envar <- newMVar newEnv
  _ <- forkIO $ inputHandler envar
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing 
               (Just "1337")
  let serveraddr = head addrinfos
  listenSock <- socket (addrFamily serveraddr) Stream defaultProtocol
  setSocketOption listenSock ReuseAddr 1
  bindSocket listenSock (addrAddress serveraddr)
  listen listenSock 5
  listenForClients listenSock envar
  return ()



-- is running forever listening for new clients
-- if a new connection is accepted, it is added to envar client array
-- and a new clientHandler is forked
listenForClients :: Socket -> MVar Env -> IO ()
listenForClients listenSock envar = forever $ do 
  (conn, _) <- accept listenSock
  env <- readMVar envar 
  case get env_acceptNew env of
    False -> logger "conection refused" >> sClose conn
    True  -> forkIO (acceptNewClient conn) >> return ()
  where 
    acceptNewClient sock = do 
      putStrLn "connection accepted"
      {-sendMsg sock (CMsgHello "jan") -- expected here-}
      msg <- recvMsg sock
      case msg of
        Just (CMsgHello nick) -> do
          env <- takeMVar envar
          now <- getCurrentTime
          let (pm, id) = addClient (get env_playerMap env) sock nick now 
          putMVar envar $ set env_playerMap pm env
          handleClient envar id
        _ -> return () 
      putStrLn "end of connection"

handleClient :: MVar Env -> Int -> IO ()
handleClient envar id = putStrLn "this is TODO next"


-- this is forked for every client
{-handleClient :: Socket -> MVar Env -> IO ()-}
{-handleClient sock envar = do-}
    {-message <- recvMessage sock-}
    {-Just c <- clientFromSocket sock envar-}
    {-case message of-}
      {-Just m  -> do handleMessage sock envar m-}
                    {-handleClient sock envar-}
      {-Nothing -> do env <- takeMVar envar-}
                    {-let pm = envPlayerMap env-}
                    {-let newPm = if envAcceptNew env-}
                      {-then -}
                      {-else -}
                    {-sClose sock-}
                    {-logger "connection closed"-}


-- this is called for every message
handleMsg :: Socket -> MVar Env -> Msg -> IO ()
handleMsg sock envar msg = case msg of

    {-TimeMsg _ -> do-}
      {-now <- getCurrentTime-}
      {-sendMsg sock TimeMessage { mTIME = now }-}
      {-logger "TimeMsg!"-}

    _ -> do
      logger "received unknown message"
