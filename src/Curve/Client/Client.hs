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

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL (($=))

import           Curve.Network.Network
import           Curve.Client.Types
import           Curve.Client.Render.Renderer
import           Curve.Game.Types
import           Curve.Game.Misc


start :: IO ()
start = do
  --init envar, connect to server and fork connection handler
  envar <- initEnvarAndConnect
  _<- forkIO $ handleConnection envar

  -- open window
  _    <- GLFW.initialize
  _    <- GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayDepthBits 8, GLFW.DisplayAlphaBits 8] GLFW.Window
  prog <- getProgName
  _    <- getArgs
  GLFW.windowTitle $= prog

  -- smoothing and antialiasing
  -- set the color to clear background
  GL.shadeModel $= GL.Smooth
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.clearColor $= GL.Color4 0 0.1 0.1 1

  -- init resources
  iores <- newIORef =<< initResources

  -- set GLFW callbacks
  GLFW.windowSizeCallback  $= windowSizeCallback envar iores
  GLFW.windowCloseCallback $= windowCloseCallback
  GLFW.mousePosCallback    $= mousePosCallback   envar iores

  forever $ do gameStep envar iores
    


-- perform one game logic and render step
gameStep :: MVar Env -> IORef Resources -> IO ()
gameStep envar iores = forever $ do

  --drop all positions except for the first three
  let chropPosList = player_posList %~ (Data.List.take 3)
  let modifyTuple (nr, (player, c)) = (nr, (chropPosList player, c))
  modifyMVar' envar $ env_playerMap %~ (Map.fromList . (map modifyTuple) . Map.toList)

  --query time if necessary  TODO this is stil crap
  env <- readMVar envar
  t   <- getCurrentTime
  let now = diffUTCTime (addUTCTime (env^.env_timer^.timer_offset) t) (env^.env_timer^.timer_start)
  modifyMVar' envar $ Control.Lens.set (env_timer . timer_now) now

  let diff = floor $ toRational $ diffUTCTime t (env^.env_timer^.timer_lastQuery)
  when ((diff >= queryOffset) && (not $ env^.env_timer^.timer_waitForResp)) $
    do modifyMVar' envar $ (env_timer . timer_waitForResp) %~ not
       modifyMVar' envar $ Control.Lens.set (env_timer . timer_lastQuery) t
       sendMsg (MsgTime t) (env^.env_socket)

  --draw on screen
  res <- readIORef iores
  env <- readMVar envar
  render res env

  {-(putStrLn . show) $ env^.env_timer^.timer_offset-}

  -- display debug infos
  {-env <- readMVar envar-}
  {-putStrLn $ "--------- " ++ (show $ L.get env_nr env) ++ " -------------"-}
  {-(putStrLn . show) env-}


-----------------------------------------------------------------------------------
-- CALLBACKS ----------------------------------------------------------------------
-----------------------------------------------------------------------------------

windowSizeCallback :: MVar Env -> IORef Resources -> GL.Size -> IO ()
windowSizeCallback envar iores size@(GL.Size w h) = do 
  GL.viewport $= (GL.Position 0 0, size)
  putStrLn $ "size " ++ (show w) ++ " " ++ (show h)

windowCloseCallback :: IO (Bool)
windowCloseCallback = do
  putStrLn "close"
  -- TODO: create shutdown field in envar
  exitWith ExitSuccess
  return True



mousePosCallback :: MVar Env -> IORef Resources -> GL.Position -> IO ()
mousePosCallback envar iores (GL.Position _x _y) = do
  env <- takeMVar envar
  t   <- getCurrentTime
  let (x,y) = (fromIntegral _x, fromIntegral _y)
  let msg = MsgPaddle { _MsgPaddle_pos = (t, x, y),
                        _MsgPaddle_nr  = env^.env_nr }
  sendMsg msg ( env^.env_socket )
  putMVar envar $ appendPaddlePos msg env



mouseMotion :: MVar Env -> GL.Position -> IO ()
mouseMotion envar (GL.Position _x _y) = do
  {-putStrLn $ show (Position x y)-}
  env <- takeMVar envar
  t   <- getCurrentTime
  let (x,y) = (fromIntegral _x, fromIntegral _y)
  let msg = MsgPaddle { _MsgPaddle_pos = (t, x, y),
                        _MsgPaddle_nr  = env^.env_nr }
  sendMsg msg (env^.env_socket )
  putMVar envar $ appendPaddlePos msg env



-----------------------------------------------------------------------------------
-- GAME NETWORK LOGIC -------------------------------------------------------------
-----------------------------------------------------------------------------------

initEnvarAndConnect :: IO (MVar Env)
initEnvarAndConnect = do
  --connect to server
  addrInfo <- getAddrInfo Nothing 
                          (Just "127.0.0.1") 
                          (Just "1337")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  sendMsg (CMsgHello "jan") sock
  --create env
  t     <- getCurrentTime
  let timer = Timer (diffUTCTime t t) t False (diffUTCTime t t) t
  newMVar $ Env Map.empty
                sock
                (-1)
                False
                timer



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

    MsgTime t -> do
              -- TODO this code looks ugly
              env         <- readMVar envar
              currentTime <- getCurrentTime
              let lastQuery  = env^.env_timer^.timer_lastQuery
              let rtt        = (diffUTCTime currentTime lastQuery) * 0.5
              let serverTime = addUTCTime rtt t
              let offset     = diffUTCTime currentTime serverTime
              modifyMVar' envar $ Control.Lens.set (env_timer . timer_waitForResp) False
              modifyMVar' envar $ Control.Lens.set (env_timer . timer_offset) offset



appendPaddlePos :: Msg -> Env -> Env
appendPaddlePos (MsgPaddle nr (t,x,y)) env = 
  let appendToPM (p, c) = (p & player_posList %~ ((t, x:.y:.()):), c)
  in  env & env_playerMap %~ (Map.adjust appendToPM nr)
