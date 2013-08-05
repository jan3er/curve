{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Client.Client where

import           System.Environment (getArgs, getProgName)
import           System.Exit
import           Control.Concurrent
import           Control.Applicative
import           Control.Monad
import           Control.Lens

import           Data.Time
import           Data.List
import           Data.Vec hiding (head, map)
{-import           Data.Maybe-}
import           Data.IORef
import qualified Data.Map.Lazy as Map

import           Network.Socket

{-import qualified          Graphics.UI.GLUT as GLUT-}
{-import           Graphics.Rendering.OpenGL-}

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))

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
  t     <- getCurrentTime
  envar <- newMVar $ Env Map.empty
                         sock
                         (-1)
                         False
                         (diffUTCTime t t) --TODO: better way for default difference?
                         t
  --fork glfw
  _ <- forkOS $ do
  {-do-}
    -- open window
    _    <- GLFW.initialize
    _    <- GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayDepthBits 8, GLFW.DisplayAlphaBits 8] GLFW.Window
    prog <- getProgName
    _    <- getArgs
    GLFW.windowTitle $= prog

    -- smoothing and antialiasing
    GL.shadeModel $= GL.Smooth
    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.lineWidth  $= 1.5

    -- set the color to clear background
    GL.clearColor $= Color4 0 0.1 0.1 1 
    
    -- set callbacks
    GLFW.windowSizeCallback $= \ size@(GL.Size w h) -> do 
        GL.viewport $= (GL.Position 0 0, size)
    
    GLFW.windowCloseCallback $= do
        putStrLn "close"
        GLFW.closeWindow
        GLFW.terminate
        _ <- exitWith ExitSuccess
        return True 
 
    ioResources   <- newIORef =<< initResources
    gameLoop envar ioResources
    {-forever $ do GL.clear [GL.ColorBuffer] >> GLFW.swapBuffers-}


    ----------------------
    {-initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]-}
    {-(progname, _) <- getArgsAndInitialize-}
    {-_             <- createWindow progname-}
    {-ioResources   <- newIORef =<< initResources-}
    {-displayCallback $= do res <- readIORef ioResources-}
                          {-env <- readMVar envar-}
                          {-display res env-}
    {-passiveMotionCallback $= Just (mouseMotion envar)-}
    {-gameLoop envar ioResources-}

  --start message handler
  sendMsg (CMsgHello "jan") sock
  handleConnection envar

gameLoop :: MVar Env -> IORef Resources -> IO ()
gameLoop envar ioResources = forever $ do
  --drop all positions except for the first three
  let chropPosList = player_posList %~ (Data.List.take 3)
  let modifyTuple (nr, (player, c)) = (nr, (chropPosList player, c))
  modifyMVar' envar $ env_playerMap %~ (Map.fromList . (map modifyTuple) . Map.toList)
  --query time if necessary  TODO this is stil crap
  {-env <- readMVar envar-}
  {-diff <- diffUTCTime (env^.env_lastTimeQuery) <$> getCurrentTime-}
  {-when (diff >= queryOffset) $ putStrLn "foo" -}

  --draw on screen
  res <- readIORef ioResources
  env <- readMVar envar
  display res env



mouseMotion :: MVar Env -> Position -> IO ()
mouseMotion envar (Position _x _y) = do
  {-putStrLn $ show (Position x y)-}
  env <- takeMVar envar
  t   <- getCurrentTime
  let (x,y) = (fromIntegral _x, fromIntegral _y)
  let msg = MsgPaddle { _MsgPaddle_pos = (t, x, y),
                        _MsgPaddle_nr  = env^.env_nr }
  sendMsg msg (env^.env_socket )
  putMVar envar $ appendPaddlePos msg env
  -- display debug infos
  {-env <- readMVar envar-}
  {-putStrLn $ "--------- " ++ (show $ L.get env_nr env) ++ " -------------"-}
  {-(putStrLn . show) env-}



handleConnection :: MVar Env -> IO ()
handleConnection envar = do
  env <- readMVar envar
  msg <- recvMsg $ env^.env_socket 
  case msg of
    Nothing -> do putStrLn "server shut down!"
    Just m  -> do handleMsg envar m
                  handleConnection envar

handleMsg :: MVar Env -> Msg -> IO ()
handleMsg envar msg = do
  case msg of
    SMsgWorld clients myNr isRunning -> do 
      env <- takeMVar envar  
      let playerMap =  Map.fromList $ map 
            (\(nr, client)
              -> let player = case Map.lookup nr (env^.env_playerMap ) of
                                Nothing -> Player { _player_posList = [] }
                                Just (p, _) -> p
                 in (nr, (player, client)) )
            clients
      --modify and put back env
      putMVar envar (env { _env_playerMap = playerMap,

                           _env_socket    = env^.env_socket ,
                           _env_nr        = myNr,
                           _env_isRunning = isRunning })
      -- display debug infos
      putStrLn $ "--------- " ++ (show myNr) ++ " -------------"
      withMVar envar $ (putStrLn . show)

    MsgPaddle _ _ -> do
      env <- takeMVar envar
      putMVar envar $ appendPaddlePos msg env


appendPaddlePos :: Msg -> Env -> Env
appendPaddlePos (MsgPaddle nr (t,x,y)) env = 
  let appendToPM (p, c) = (p & player_posList %~ ((t, x:.y:.()):), c)
  in  env & env_playerMap %~ (Map.adjust appendToPM nr)
