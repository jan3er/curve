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
  addrInfo <- getAddrInfo Nothing 
                        (Just "127.0.0.1") 
                        (Just "1337")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  envar <- newMVar $ Env { _env_playerMap = Map.empty,
                           _env_socket    = Just sock }
  connect sock (addrAddress serverAddr)
  sendMsg (CMsgHello "jan") sock
  _ <- forkIO $ handleConnection envar

  (progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= display
  passiveMotionCallback $= Just (mouseMotion envar)
  mainLoop

  putStrLn "exit"



mouseMotion :: MVar Env -> Position -> IO ()
mouseMotion envar (Position x y) = do
  putStrLn $ show (Position x y)
  env <- readMVar envar
  let msg = CMsgPaddle { _msg_pos = (fromIntegral x, fromIntegral y) }
  sendMsg msg (fromJust $ L.get env_socket env)



handleConnection :: MVar Env -> IO ()
handleConnection envar = do
  env <- readMVar envar
  msg <- recvMsg $ fromJust $ L.get env_socket env
  case msg of
    Nothing   -> do putStrLn "panic! server shut down!"
    Just msg  -> do handleMsg envar msg
                    handleConnection envar

handleMsg :: MVar Env -> Msg -> IO ()
handleMsg envar msg = do 
  case msg of
    SMsgWorld world -> do 
      env <- takeMVar envar  
      let playerMap =  Map.fromList $ map 
            (\(id, client)
              -> let player = case Map.lookup id (L.get env_playerMap env) of
                                Nothing -> Player { playerPosition = (0.2,0.2) }
                                Just (p, _) -> p
                 in (id, (player, client)) )
            world
      putMVar envar (L.set env_playerMap playerMap env)
  env <- readMVar envar
  (putStrLn . show) env

