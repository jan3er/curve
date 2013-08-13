{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Client.Client where

import           System.Environment (getArgs, getProgName)
import           System.Exit
import           Control.Concurrent
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Lens

import           Debug.Trace

import           Data.Time
import           Data.List
import           Data.Maybe
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




-------------------------------------------------------------------------------
-- establishing a connection --------------------------------------------------
-------------------------------------------------------------------------------

establishConnection :: String -> IO (MVar [Msg], Env)
establishConnection playerName = do
    addrInfo <- getAddrInfo Nothing 
                          (Just "127.0.0.1") 
                          (Just "1337")
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    sendMsg (CMsgHello playerName) sock

    msgQueue <- newMVar []
    _ <- forkIO $ putInMsgQueue msgQueue sock

    env <- initEnv sock
    return (msgQueue, env)
    where
        putInMsgQueue :: MVar [Msg] -> Socket -> IO ()
        putInMsgQueue msgQueue sock = do
            mmsg <- recvMsg sock
            case mmsg of
                Nothing  ->    sClose sock
                Just msg -> do queue <- takeMVar msgQueue
                               putMVar msgQueue (msg : queue)
                               putInMsgQueue msgQueue sock

        initEnv :: Socket -> IO Env
        initEnv sock = do
            t <- getCurrentTime
            let timer  = Timer t t t False
            let window = Window $ GL.Position 0 0
            return $ Env Map.empty
                     sock
                     (-1)
                     False
                     timer
                     window
  

-------------------------------------------------------------------------------
-- pure message handlers ------------------------------------------------------
-------------------------------------------------------------------------------

handleMsg :: Msg -> Env -> Env
handleMsg msg env = 
    case trace ("=> incomming: " ++ show msg) msg of
        SMsgWorld clients myNr isRunning -> 
            let getPlayer nr = 
                    case Map.lookup nr (env^.env_playerMap) of
                        Nothing     -> Player []
                        Just (p, _) -> p
                playerMap =  
                    Map.fromList $ map
                    (\(nr, client) -> (nr, (getPlayer nr, client)))
                    clients
            in 
            env_nr        .~ myNr      $
            env_isRunning .~ isRunning $
            env_playerMap .~ playerMap $
            env

        MsgPaddle nr (t,x,y) -> 
            appendPaddlePos nr (t,x,y) env

        MsgTime t ->
            let timer = env^.env_timer
                mediumLocalTime = 
                    addUTCTime 
                    (0.5 * diffUTCTime (timer^.timer_localTime) (timer^.timer_lastQuery))
                    (timer^.timer_lastQuery) 
                newReferenceTime =
                    addUTCTime (-1*t) mediumLocalTime 
            in
            env_timer.timer_referenceTime .~ newReferenceTime $
            env_timer.timer_waitForResp   .~ False            $
            env
        
        _ -> error "Client.handleMsg"
            



appendPaddlePos :: Int -> (NominalDiffTime, Float, Float) -> Env -> Env
appendPaddlePos nr posTuple = 
  let appendToPM = _1.player_posList %~ (posTuple:)
  in  env_playerMap %~ Map.adjust appendToPM nr

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
{-StateT (return . runState foo)-}
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

mouseInput :: StateT Env IO ()
mouseInput = do
    oldPos                  <- use $ env_window.window_mousePos
    newPos@(GL.Position _x _y) <- liftIO $ GL.get GLFW.mousePos
    env_window.window_mousePos .= newPos
    if newPos /= oldPos 
        then return ()
        else do 
             let (x, y) = (fromIntegral _x, fromIntegral _y)
             timer      <- use env_timer
             globalTime <- liftIO $ toGlobalTime timer <$> getCurrentTime 
             myNr       <- use env_nr
             modify $ appendPaddlePos myNr (globalTime, x, y)
        
             sock <- use env_socket
             liftIO $ sendMsg (MsgPaddle myNr (globalTime, x, y)) sock
    


-- keep timer up to date and in sync
updateTimer :: StateT Env IO ()
updateTimer = do
    -- set localTime
    currentTime <- liftIO getCurrentTime
    env_timer.timer_localTime .= currentTime

    -- maybe send message
    let queryInterval = 2
    timer       <- use env_timer
    let diff :: Float = realToFrac $ diffUTCTime (timer^.timer_localTime) (timer^.timer_lastQuery)
    when ((diff >= queryInterval) && not (timer^.timer_waitForResp)) $ do
        env_timer.timer_waitForResp .= True
        env_timer.timer_lastQuery   .= currentTime

        sock <- use env_socket
        liftIO $ sendMsg (MsgTime 0) sock


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
    (msgQueue, startEnv) <- establishConnection "jan"

    
    -- start loop
    _ <- flip execStateT (startEnv, startRes) $ do 
        forever $ do
            (env1, res1) <- get
            env2 <- liftIO $ execStateT (stepEnv msgQueue) env1
            res2 <- liftIO $ execStateT (renderStep  env2) res1
            put (env2, res2)
   
            liftIO GLFW.swapBuffers
        
    return ()

stepEnv :: MVar [Msg] -> StateT Env IO ()
stepEnv msgQueue = do

    -- process incomming messages (GLFW and network)
    queue <- lift $ swapMVar msgQueue []
    modify $ \env -> foldl (flip handleMsg) env queue 
    mouseInput

    -- keep timer up to date and in sync
    updateTimer

    -- delete all but the last three paddle positions
    modify $ env_playerMap.mapped._1.player_posList %~ Data.List.take 3

    {-env <- get-}
    {-liftIO $ putStrLn $ show env-}

    
