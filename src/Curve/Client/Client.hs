{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Client.Client where

import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Data.Time
import           Data.List
import           Data.Maybe
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
                           _env_paddlePos = (0.5,0.5),
                           _env_isRunning = False }
  --fork glut
  _ <- forkOS $ do
    initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
    (progname, _) <- getArgsAndInitialize
    createWindow "Hello World"
    displayCallback $= display envar
    passiveMotionCallback $= Just (mouseMotion envar)
    mainLoop
  --start message handler
  sendMsg (CMsgHello "jan") sock
  handleConnection envar



mouseMotion :: MVar Env -> Position -> IO ()
mouseMotion envar (Position x y) = do
  {-putStrLn $ show (Position x y)-}
  env <- takeMVar envar
  let floatTupe = (fromIntegral x, fromIntegral y)
  let msg = MsgPaddle { _MsgPaddle_pos = floatTupe,
                        _MsgPaddle_id  = -1 }
  sendMsg msg (L.get env_socket env)
  putMVar envar (set env_paddlePos floatTupe env)



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
    SMsgWorld clients id isRunning -> do 
      env <- takeMVar envar  
      let playerMap =  Map.fromList $ map 
            (\(id, client)
              -> let player = case Map.lookup id (L.get env_playerMap env) of
                                Nothing -> Player { playerPosition = (0.2,0.2) }
                                Just (p, _) -> p
                 in (id, (player, client)) )
            clients
      --modify and put back env
      putMVar envar (env { _env_playerMap = playerMap,
                           _env_socket    = L.get env_socket env,
                           _env_isRunning = isRunning })

    MsgPaddle id pos -> do
      (putStrLn . show) (id, pos)
  env <- readMVar envar
  (putStrLn . show) env


