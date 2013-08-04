{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Client.Client where

import           System.IO
import           Control.Concurrent
import           Control.Applicative
import           Control.Monad
import           Control.Lens

import           Data.Time
import           Data.List
import           Data.Vec hiding (head, map)
import           Data.Maybe
import           Data.IORef
import qualified Data.Map.Lazy as Map

import           Network.Socket

import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT

import           Curve.Network.Network
import           Curve.Client.Types
import           Curve.Client.Render.Renderer
import           Curve.Game.Types
import           Curve.Game.Misc


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
      --drop all positions except for the first three
      {-let chropPosList = L.modify player_posList (Data.List.take 3)-}
      let chropPosList = player_posList %~ (Data.List.take 3)
      let modifyTuple (id, (player, c)) = (id, (chropPosList player, c))
      env <- takeMVar envar
      putMVar envar $ env & env_playerMap %~ (Map.fromList . (map modifyTuple) . Map.toList)      
      --draw on screen and do event handling
      render ioResources =<< readMVar envar

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
                        _MsgPaddle_id  = env^.env_id }
  sendMsg msg (env^.env_socket )
  putMVar envar $ appendPaddlePos msg env
  -- display debug infos
  {-env <- readMVar envar-}
  {-putStrLn $ "--------- " ++ (show $ L.get env_id env) ++ " -------------"-}
  {-(putStrLn . show) env-}



handleConnection :: MVar Env -> IO ()
handleConnection envar = do
  env <- readMVar envar
  msg <- recvMsg $ env^.env_socket 
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
              -> let player = case Map.lookup id (env^.env_playerMap ) of
                                Nothing -> Player { _player_posList = [] }
                                Just (p, _) -> p
                 in (id, (player, client)) )
            clients
      --modify and put back env
      putMVar envar (env { _env_playerMap = playerMap,

                           _env_socket    = env^.env_socket ,
                           _env_id        = myId,
                           _env_isRunning = isRunning })
      -- display debug infos
      putStrLn $ "--------- " ++ (show myId) ++ " -------------"
      env <- readMVar envar
      (putStrLn . show) env

    MsgPaddle _ _ -> do
      env <- takeMVar envar
      putMVar envar $ appendPaddlePos msg env

  {-env <- readMVar envar-}
  {-(putStrLn . show) env-}


appendPaddlePos :: Msg -> Env -> Env
appendPaddlePos (MsgPaddle id (t,x,y)) env = 
  let appendToPM (p, c) = (p & player_posList %~ ((t, x:.y:.()):), c)
  in  env & env_playerMap %~ (Map.adjust appendToPM id)
