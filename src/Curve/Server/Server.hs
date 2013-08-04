{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Server.Server where

import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Control.Applicative
import           Data.Time
import           Data.List
import           Data.Maybe
import qualified Data.Map.Lazy as Map
import           Network.Socket

import           Control.Lens

import           Curve.Server.Types
import           Curve.Server.Misc
import           Curve.Network.Network
import           Curve.Game.Misc




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
      {-let modifiedEnv = modify env_isRunning not env -}
      let modifiedEnv = env & env_isRunning %~ not
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
  case env^.env_isRunning  of
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
          let (pm, id) = addClient (envOld^.env_playerMap) sock nick now 
          let env = envOld & Control.Lens.set env_playerMap pm
          -- TODO:
          {-let env = envOld & env_playerMap ^~ pm -}
          --broadcast changed world
          broadcastWorld env
          --put envar back and start handler
          putMVar envar env
          handleClient envar id
        _ -> putStrLn $ ( show msg )
      putStrLn "end of connection"



handleClient :: MVar Env -> Int -> IO ()
handleClient envar id = do
  env <- readMVar envar
  let Just (_, Just client) = Map.lookup id (env^.env_playerMap )
  msg <- recvMsg $ client^.scl_socket 
  case msg of
    --connection closed? -> kill or delete client
    Nothing -> do let f = if env^.env_isRunning then killClient else Map.delete
                  {-modifyMVar' envar $ modify env_playerMap (f id)-}
                  modifyMVar' envar $ env_playerMap %~ (f id)
                  withMVar    envar broadcastWorld

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


-------------------------MESSAGE HANDLING---------------------------------

-- this is called for every incomming message
handleMsg :: MVar Env -> Int -> Msg -> IO ()
handleMsg envar id msg = case msg of
    
    MsgPaddle _ (t, x, y) -> do 
      env <- takeMVar envar
      putStrLn $ (show x) ++ " " ++ (show y)
      broadcastPaddlePos env id msg
      putMVar envar env 
    _ -> do
      logger "received unknown message"

-- boadcast a SMsgWorld to all connected clients
broadcastWorld :: Env -> IO ()
broadcastWorld env =
  let msgClients = map
        {-(\(id, (_, c)) -> (id, maybe Nothing (Just . get scl_client) c))-}
        (\(id, (_, c)) -> (id, (view scl_client) <$> c))
        (Map.toList $ env^.env_playerMap )
  in do
  mapM_ (\id -> sendMsg (msgWorld msgClients id) ((clientById env id)^.scl_socket )) 
        (getConnectedClients env)
  where
    msgWorld msgClients id = 
      SMsgWorld { 
        _SMsgWorld_clients   = msgClients,
        _SMsgWorld_clientId  = id,
        _SMsgWorld_isRunning = env^.env_isRunning  
      }

-- boadcast paddlePos to all clients except for the one with ID=id
-- accept only MsgPaddle
broadcastPaddlePos :: Env -> Int -> Msg -> IO ()
broadcastPaddlePos env id (MsgPaddle _ pos) = 
  let msgPaddlePos = MsgPaddle { _MsgPaddle_id = id, _MsgPaddle_pos = pos }
  in do
  mapM_ (\id -> sendMsg msgPaddlePos ((clientById env id)^.scl_socket ))
        (filter (/= id) $ getConnectedClients env)


-- return ids of all connected clients
getConnectedClients :: Env -> [Int]
getConnectedClients env = 
  let isConnected (_, (_, c)) = maybe False (^.(scl_client.cl_isAlive)) c
  in  map fst $ filter isConnected (Map.toList $ env^.env_playerMap)

-- returns the socket by id
clientById env id = (fromJust . snd) (fromJust $ Map.lookup id (env^.env_playerMap ))
