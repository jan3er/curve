{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Server.Server where

import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Data.Time
import           Data.List
import           Data.Maybe
import qualified Data.Map.Lazy as Map
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
    "toggle"  -> putStrLn "toggling isRunning" >> toggleIsRunning
    "env"     -> readMVar envar >>= putStrLn . show
    _         -> putStrLn "unknown"
  where 
    toggleIsRunning = do 
      env <- takeMVar envar
      let modifiedEnv = modify env_isRunning not env 
      broadcastWorld modifiedEnv
      putMVar envar modifiedEnv





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
  case get env_isRunning env of
    True  -> logger "conection refused" >> sClose conn
    False -> forkIO (acceptNewClient conn) >> return ()
  where 
    acceptNewClient sock = do 
      putStrLn "connection accepted"
      {-sendMsg sock (CMsgHello "jan") -- expected here-}
      msg <- recvMsg sock
      case msg of
        Just (CMsgHello nick) -> do
          --take envar and add client to env
          envOld <- takeMVar envar
          now <- getCurrentTime
          let (pm, id) = addClient (get env_playerMap envOld) sock nick now 
          let env = set env_playerMap pm envOld
          --broadcast changed world
          broadcastWorld env
          --put envar back and start handler
          putMVar envar env
          handleClient envar id

        _ -> putStrLn $ ( show msg )

      putStrLn "end of connection"

-- boadcast a SMsgWorld to all clients
broadcastWorld :: Env -> IO ()
broadcastWorld env = do
  let socks = mapMaybe 
        (\(_, (_, c))
           -> if isNothing c 
              then Nothing 
              else Just (get scl_socket (fromJust c)))
        (Map.toList $ get env_playerMap env)
  let clients = map 
        (\(id, (_, c)) 
          -> if isNothing c 
             then (id, Nothing) 
             else (id, Just $ get scl_client (fromJust c)))
        (Map.toList $ get env_playerMap env)
  mapM (sendMsg (SMsgWorld { _msg_clients   = clients, _msg_isRunning = get env_isRunning env })) socks
  return ()



handleClient :: MVar Env -> Int -> IO ()
handleClient envar id = do
  env <- readMVar envar
  let Just (_, Just client) = Map.lookup id (get env_playerMap env)
  msg <- recvMsg $ get scl_socket client
  case msg of
    --connection closed? -> kill or delete client
    Nothing -> do env <- takeMVar envar 
                  let f = if get env_isRunning env then killClient else Map.delete
                  putMVar envar $ modify env_playerMap (f id) env
                  --TODO close sock

    -- TODO
    Just msg  -> do handleMsg envar id msg
                    handleClient envar id


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


-- this is called for every incomming message
handleMsg :: MVar Env -> Int -> Msg -> IO ()
handleMsg sock envar msg = case msg of
    
    CMsgPaddle (x, y) -> putStrLn $ (show x) ++ " " ++ (show y)

    {-TimeMsg _ -> do-}
      {-now <- getCurrentTime-}
      {-sendMsg sock TimeMessage { mTIME = now }-}
      {-logger "TimeMsg!"-}

    _ -> do
      logger "received unknown message"
