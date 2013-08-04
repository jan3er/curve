{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Client.Client where

import           System.IO
import           Control.Concurrent
import           Control.Applicative
import           Control.Monad
import           Data.Time
import           Data.List
import           Data.Vec hiding (head, map)
import           Data.Maybe
import           Data.IORef
import           Data.Label as L
import qualified Data.Map.Lazy as Map
import           Network.Socket
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT

import           Curve.Network.Network
import           Curve.Client.Types
import           Curve.Client.Render.Renderer
import           Curve.Game.Types


start :: IO ()
start = do
  --connect to server
  addrInfo <- getAddrInfo Nothing 
                        (Just "127.0.0.1") 
                        (Just "1337")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  --create env
  envar <- newMVar $ Env { _env_playerMap = Map.empty,
                           _env_socket    = sock,
                           _env_id        = -1,
                           _env_isRunning = False }
  --fork glut
  _ <- forkOS $ do
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    (progname, _) <- getArgsAndInitialize
    _ <- createWindow "curve"
    ioResources <- newIORef =<< initResources
    displayCallback $= do res <- readIORef ioResources
                          env <- readMVar envar
                          display res env
    passiveMotionCallback $= Just (mouseMotion envar)
    forever $ do
      env <- takeMVar envar
      --drop all positions except for the first three
      let chropPosList = L.modify player_posList (Data.List.take 3)
      let modifyTuple (id, (player, c)) = (id, (chropPosList player, c))
      putMVar envar $ L.modify env_playerMap (Map.fromList . (map modifyTuple) . Map.toList) env
      --draw on screen and do event handling
      render ioResources env

  --start message handler
  sendMsg (CMsgHello "jan") sock
  handleConnection envar



mouseMotion :: MVar Env -> Position -> IO ()
mouseMotion envar (Position _x _y) = do
  {-putStrLn $ show (Position x y)-}
  env <- takeMVar envar
  t <- getCurrentTime
  let (x,y) = (fromIntegral _x, fromIntegral _y)
  let msg = MsgPaddle { _MsgPaddle_pos = (t, x, y),
                        _MsgPaddle_id  = L.get env_id env }
  sendMsg msg (L.get env_socket env)
  putMVar envar $ appendPaddlePos msg env
  -- display debug infos
  {-env <- readMVar envar-}
  {-putStrLn $ "--------- " ++ (show $ L.get env_id env) ++ " -------------"-}
  {-(putStrLn . show) env-}



handleConnection :: MVar Env -> IO ()
handleConnection envar = do
  env <- readMVar envar
  msg <- recvMsg $ L.get env_socket env
  case msg of
    Nothing   -> do putStrLn "server shut down!"
    Just msg  -> do handleMsg envar msg
                    handleConnection envar

handleMsg :: MVar Env -> Msg -> IO ()
handleMsg envar msg = do
  case msg of
    SMsgWorld clients myId isRunning -> do 
      env <- takeMVar envar  
      let playerMap =  Map.fromList $ map 
            (\(id, client)
              -> let player = case Map.lookup id (L.get env_playerMap env) of
                                Nothing -> Player { _player_posList = [] }
                                Just (p, _) -> p
                 in (id, (player, client)) )
            clients
      --modify and put back env
      putMVar envar (env { _env_playerMap = playerMap,
                           _env_socket    = L.get env_socket env,
                           _env_id        = myId,
                           _env_isRunning = isRunning })
      -- display debug infos
      {-putStrLn $ "--------- " ++ (show myId) ++ " -------------"-}
      {-env <- readMVar envar-}
      {-(putStrLn . show) env-}

    MsgPaddle _ _ -> do
      env <- takeMVar envar
      putMVar envar $ appendPaddlePos msg env

  {-env <- readMVar envar-}
  {-(putStrLn . show) env-}


appendPaddlePos :: Msg -> Env -> Env
appendPaddlePos (MsgPaddle id (t,x,y)) env = 
  let appendToPM (p, c) = (L.modify player_posList ((t, x:.y:.()):) p, c)
  in  modify env_playerMap (Map.adjust appendToPM id) env
