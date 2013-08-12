{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Server.Server where

import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Control.Applicative
import           Data.Time
import           Data.Maybe
import qualified Data.Map.Lazy as Map
{-import           Data.List-}

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



-- creates a new clean env
newEnv :: IO Env
newEnv =
    Env <$> pure Map.empty
        <*> pure False
        <*> getCurrentTime
             

-- the server entry point
start :: IO ()
start = withSocketsDo $ do
  putStrLn "server started"
  hSetBuffering stdout LineBuffering
  envar <- newMVar =<< newEnv
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
          let (pm, nr) = addClient (envOld^.env_playerMap) sock nick now 
          let env = envOld & Control.Lens.set env_playerMap pm
          -- TODO:
          {-let env = envOld & env_playerMap ^~ pm -}
          --broadcast changed world
          broadcastWorld env
          --put envar back and start handler
          putMVar envar env
          handleClient envar nr
        _ -> putStrLn $ ( show msg )
      putStrLn "end of connection"



handleClient :: MVar Env -> Int -> IO ()
handleClient envar nr = do
  env <- readMVar envar
  let Just (_, Just client) = Map.lookup nr (env^.env_playerMap )
  msg <- recvMsg $ client^.scl_socket 
  case msg of
    --connection closed? -> kill or delete client
    Nothing -> do let f = if env^.env_isRunning then killClient else Map.delete
                  {-modifyMVar' envar $ modify env_playerMap (f nr)-}
                  modifyMVar' envar $ env_playerMap %~ (f nr)
                  withMVar    envar broadcastWorld

    -- TODO
    Just m  -> do handleMsg envar nr m
                  handleClient envar nr


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
handleMsg envar nr msg = do
  putStrLn $ show msg
  case msg of
    MsgPaddle _ (_, x, y) -> do env <- takeMVar envar
                                putStrLn $ (show x) ++ " " ++ (show y)
                                broadcastPaddlePos env nr msg
                                putMVar envar env 

    MsgTime _             -> do env <- readMVar envar
                                t <- getCurrentTime
                                let m = MsgTime $ diffUTCTime t (env^.env_startTime)
                                sendMsg m $ (clientByNr env nr)^.scl_socket

    _ -> do logger "received unknown message" >> (putStrLn . show) msg

-- boadcast a SMsgWorld to all connected clients
broadcastWorld :: Env -> IO ()
broadcastWorld env =
  let msgClients = map
        {-(\(nr, (_, c)) -> (nr, maybe Nothing (Just . get scl_client) c))-}
        (\(nr, (_, c)) -> (nr, (view scl_client) <$> c))
        (Map.toList $ env^.env_playerMap )
  in do
  mapM_ (\nr -> sendMsg (msgWorld msgClients nr) ((clientByNr env nr)^.scl_socket )) 
        (getConnectedClients env)
  where
    msgWorld msgClients nr = 
      SMsgWorld { 
        _SMsgWorld_clients   = msgClients,
        _SMsgWorld_clientNr  = nr,
        _SMsgWorld_isRunning = env^.env_isRunning  
      }

-- boadcast paddlePos to all clients except for the one with ID=nr
-- accept only MsgPaddle
-- TODO make more clear which nr is taken
broadcastPaddlePos :: Env -> Int -> Msg -> IO ()
broadcastPaddlePos env nr (MsgPaddle _ pos) = 
  let msgPaddlePos = MsgPaddle { _MsgPaddle_nr = nr, _MsgPaddle_pos = pos }
  in do
  mapM_ (\nr' -> sendMsg msgPaddlePos ((clientByNr env nr')^.scl_socket ))
        (filter (/= nr) $ getConnectedClients env)


-- return nrs of all connected clients
getConnectedClients :: Env -> [Int]
getConnectedClients env = 
  let isAlive (_, (_, c)) = maybe False (^.(scl_client.cl_isAlive)) c
  in  map fst $ filter isAlive (Map.toList $ env^.env_playerMap)

-- returns the socket by nr
clientByNr :: Env -> Int -> SClient
clientByNr env nr = (fromJust . snd) (fromJust $ Map.lookup nr (env^.env_playerMap ))
