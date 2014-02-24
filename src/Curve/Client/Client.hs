{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Client.Client where

import           System.Environment (getProgName)
import           System.IO
import           Control.Concurrent
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Lens

{-import           Debug.Trace-}

import           Data.Time
{-import           Data.List-}
import           Data.Maybe
import qualified Data.Map.Lazy as Map

import           Network.Socket

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.Rendering.OpenGL (($=))

import           Curve.Game.Network
import           Curve.Client.Types
import           Curve.Client.Render.Renderer
import           Curve.Game.Player
import           Curve.Game.Ball
{-import           Curve.Game.Wall-}



import qualified Curve.Game.Math as M
{-import           Curve.Game.Math (Vec3)-}

import           Curve.Client.Timer as Timer
{-import           Curve.Game.Ball    as Ball -}
import           Curve.Game.Player  as Player
import           Curve.Game.Paddle  as Paddle
import           Curve.Game.Wall    as Wall
import           Curve.Game.World   as World


-- run state in stateT monad
{-StateT (return . runState foo)-}

-------------------------------------------------------------------------------
-- establishing a connection --------------------------------------------------
-------------------------------------------------------------------------------

establishConnection :: String -> MsgHandler Env -> IO (MVar Env)
establishConnection playerName msgHandler = do
    --set buffering type maybe?
    {-hSetBuffering stdout LineBuffering-}
    addrInfo <- getAddrInfo Nothing 
                          (Just "127.0.0.1") 
                          (Just "1337")
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    putMsg hdl (CMsgHello playerName)

    mEnv <- newMVar =<< initEnv hdl
    forkMsgHandler mEnv msgHandler
    return mEnv


forkMsgHandler :: MVar Env -> MsgHandler Env -> IO ()
forkMsgHandler mEnv handler = do
    handle <- _env_handle <$> readMVar mEnv
    _<- forkIO $ getMsgAndHandle mEnv handle handler
    return ()


-------------------------------------------------------------------------------
-- pure message handlers ------------------------------------------------------
-------------------------------------------------------------------------------

handleMsgPure :: MsgHandlerPure Env
handleMsgPure msg = do
    env <- get
    case msg of
        -- the world has changed, adapt local env/world to changes
        SMsgWorld clients myNr isRunning -> do

            env_nr        .= myNr    
            env_isRunning .= isRunning
 
            -- put all clients in env_clientMap 
            let getClient (nr, maybeClient) = maybeClient >>= \c -> Just (nr,c)
            env_clientMap .= 
                ( Map.fromList 
                . mapMaybe getClient
                $ clients )

            -- update env_playerMap, add empty players for new player nrs
            let getPlayer nr = fromMaybe Player.new (Map.lookup nr (env^.env_world^._playerMap))
            env_world._playerMap .= 
                ( Map.fromList
                . map (\nr -> (nr, getPlayer nr))
                . map fst
                $ clients )

            -- do not reply
            return []


        MsgPaddle nr (t,x,y) -> do
            modify $ appendPaddlePos nr (t,x,y)
            return []

        MsgTime t -> do
            env_timer %= Timer.serverUpdate (MsgTime t)
            return []
        
        SMsgBall t (p1,p2,p3) (d1,d2,d3) (s1,s2,s3) -> do
            env_world._ball .= Ball t (M.mkVec3 p1 p2 p3) (M.mkVec3 d1 d2 d3) (M.mkVec3 s1 s2 s3) 0
            return []

        _ -> error "Error: Client.handleMsg"
            



appendPaddlePos :: Int -> (NominalDiffTime, Float, Float) -> Env -> Env
appendPaddlePos nr posTuple = 
  let appendToPaddle = _paddle %~ (Paddle.insert posTuple)
  in  env_world._playerMap %~ Map.adjust appendToPaddle nr

-------------------------------------------------------------------------------
-- MOST CODE ------------------------------------------------------------------
-------------------------------------------------------------------------------

-- set env's windowsize to actual windowsize
windowResize :: StateT Env IO ()
windowResize = do
    size <- liftIO $ GL.get GLFW.windowSize
    env_window.window_size .= size
    liftIO $ GL.viewport $= (GL.Position 0 0, size)

-- set mouse pos to server
-- TODO: return bool if changed instead of actually sending the message?
mouseInput :: StateT Env IO ()
mouseInput = do
    oldPos                     <- use $ env_window.window_mousePos
    newPos@(GL.Position _x _y) <- liftIO $ GL.get GLFW.mousePos
    env_window.window_mousePos .= newPos
    when (newPos /= oldPos) $ do
        let (x, y) = (fromIntegral _x, fromIntegral _y)
        env        <- get
        globalTime <- Timer.getTime <$> use env_timer
        modify $ appendPaddlePos (env^.env_nr) (globalTime, x, y)
        liftIO $ putMsg (env^.env_handle) (MsgPaddle (env^.env_nr) (globalTime, x, y))


-- keep timer up to date and in sync
updateTimer :: StateT Env IO ()
updateTimer = do
    oldTimer <- use env_timer
    timer <- liftIO $ Timer.ioUpdate oldTimer
    env_timer .= timer

initGL :: IO Resources 
initGL = do
    -- open window and init callbacks
    _ <- GLFW.initialize
    _ <- GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayDepthBits 8, GLFW.DisplayAlphaBits 8] GLFW.Window
    ($=) GLFW.windowTitle =<< getProgName

    initResources 
    

start :: IO ()
start = do

    -- get resources
    startRes <- initGL

    -- connect to server
    mEnv <- establishConnection "jan" (return (), handleMsgPure, return ())

   
    modifyMVar_ mEnv $ execStateT $ do
        let walls = (fst $ Wall.initArena 3 3 5)
        env_world._extraWalls .= walls

 

    
    -- start loop
    _ <- forever $ flip execStateT startRes $ do
        liftIO $ modifyMVar_ mEnv $ execStateT stepEnv
        renderStep =<< liftIO (readMVar mEnv)

    return ()

stepEnv :: StateT Env IO ()
stepEnv = do
    -- react to mouse movement
    mouseInput
    windowResize

    -- keep timer up to date and in sync
    updateTimer

    -- delete all but the last three paddle positions
    modify $ env_world._playerMap.mapped._paddle  %~ Paddle.clamp

    {-env <- get-}
    {-liftIO $ putStrLn $ show env-}

    
