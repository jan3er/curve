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

establishConnection :: String -> MsgHandler Env -> IO (MVar Env)
establishConnection playerName msgHandler =
    let initEnv sock = do
            t <- getCurrentTime
            let timer  = Timer t t t False
            let window = Window $ GL.Position 0 0
            return $ Env Map.empty
                     sock
                     (-1)
                     False
                     timer
                     window
    in do
    --set buffering type maybe?
    {-hSetBuffering stdout LineBuffering-}
    addrInfo <- getAddrInfo Nothing 
                          (Just "127.0.0.1") 
                          (Just "1337")
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    sendMsg (CMsgHello playerName) sock

    mEnv <- newMVar =<< initEnv sock
    forkMsgHandler mEnv msgHandler
    return mEnv


forkMsgHandler :: MVar Env -> MsgHandler Env -> IO ()
forkMsgHandler mEnv handler = do
    sock <- _env_socket <$> readMVar mEnv
    _<- forkIO $ recvMsgAndHandle mEnv sock handler
    return ()


-------------------------------------------------------------------------------
-- pure message handlers ------------------------------------------------------
-------------------------------------------------------------------------------

handleMsg :: MsgHandler Env
handleMsg msg = do
    env <- get
    case msg of
        SMsgWorld clients myNr isRunning -> 
            let getPlayer nr = 
                    case Map.lookup nr (env^.env_playerMap) of
                        Nothing     -> Player []
                        Just (p, _) -> p
                playerMap =  
                    Map.fromList $ map
                    (\(nr, client) -> (nr, (getPlayer nr, client)))
                    clients
            in do
            env_nr        .= myNr     
            env_isRunning .= isRunning
            env_playerMap .= playerMap
            return []


        MsgPaddle nr (t,x,y) -> do
            modify $ appendPaddlePos nr (t,x,y)
            return []

        MsgTime t ->
            let timer = env^.env_timer
                mediumLocalTime = 
                    addUTCTime 
                    (0.5 * diffUTCTime (timer^.timer_localTime) (timer^.timer_lastQuery))
                    (timer^.timer_lastQuery) 
                newReferenceTime =
                    addUTCTime (-1*t) mediumLocalTime 
            in do
            env_timer.timer_referenceTime .= newReferenceTime
            env_timer.timer_waitForResp   .= False           
            return []
        
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
    oldPos                     <- use $ env_window.window_mousePos
    newPos@(GL.Position _x _y) <- liftIO $ GL.get GLFW.mousePos
    env_window.window_mousePos .= newPos
    when (newPos /= oldPos) $ do
        let (x, y) = (fromIntegral _x, fromIntegral _y)
        timer      <- use $ env_timer
        globalTime <- liftIO $ toGlobalTime timer <$> getCurrentTime 
        myNr       <- use $ env_nr
        modify $ appendPaddlePos myNr (globalTime, x, y)

        sock <- use $ env_socket
        liftIO $ sendMsg (MsgPaddle myNr (globalTime, x, y)) sock
    


-- keep timer up to date and in sync
updateTimer :: StateT Env IO ()
updateTimer = do
    -- set localTime
    currentTime <- liftIO $ getCurrentTime
    env_timer.timer_localTime .= currentTime

    -- maybe send message
    let queryInterval = 2
    timer       <- use $ env_timer
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
    mEnv <- establishConnection "jan" handleMsg

    
    -- start loop
    _ <- forever $ flip execStateT startRes $ do
        liftIO $ modifyMVar_ mEnv $ execStateT stepEnv
        renderStep =<< liftIO (readMVar mEnv)

    return ()

stepEnv :: StateT Env IO ()
stepEnv = do
    -- react to mouse movement
    mouseInput

    -- keep timer up to date and in sync
    updateTimer

    -- delete all but the last three paddle positions
    modify $ env_playerMap.mapped._1.player_posList %~ Data.List.take 3

    {-env <- get-}
    {-liftIO $ putStrLn $ show env-}

    
