{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Server.Server where

import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Control.Applicative
{-import           Debug.Trace-}
import           Data.Time
{-import           Data.Maybe-}
import           Data.List
import qualified Data.Map.Lazy as Map
{-import           Data.List-}

import           Network.Socket

import           Control.Lens

import           Curve.Server.Env
import           Curve.Server.ClientMap
import           Curve.Server.Timer as Timer
import           Curve.Game.Player as Player
import           Curve.Game.Wall as Wall
import           Curve.Game.World as World
import           Curve.Game.Ball as Ball
import qualified Curve.Game.Math as M

import           Curve.Game.Network
import           Curve.Game.Timer

----------------------------------------

type MsgHandlerServer a = (MsgHandlerPre a, (Int -> MsgHandlerPure a), MsgHandlerPost a)

-------------------------------------------------------------------------------
-- IO message handlers --------------------------------------------------------
-------------------------------------------------------------------------------

handleMsgPre :: MsgHandlerPre Env
handleMsgPre = assign env_timer =<< liftIO . Timer.ioUpdate =<< use env_timer

handleMsgPost :: MsgHandlerPost Env
handleMsgPost = return ()

-------------------------------------------------------------------------------
-- pure message handlers ------------------------------------------------------
-------------------------------------------------------------------------------

-- this is called for every incomming message
handleMsgPure :: Int -> MsgHandlerPure Env
handleMsgPure nr msg = do
    case msg of

        MsgPaddle _ (t, x, y) -> do 
            getPaddleBroadcast nr (t,x,y) <$> get

        CMsgHello nick -> do
            -- TODO: is this correct?
            let updateName = fmap $ scl_client.cl_nick .~ nick
            modify $ env_clientMap %~ Map.alter updateName nr
            getWorldBroadcast <$> get


        MsgTime _ -> do 
            msgOut <- MsgTime . getTime <$> use env_timer
            sock <- view scl_handle . clientFromNr nr . view env_clientMap <$> get
            return [(sock, msgOut)]

        _ -> error "Server.handleMsg: no handler for this message"


-- TODO: put this somewhere else
getWorldBroadcast :: Env -> [(Handle,Msg)]
getWorldBroadcast env = 
    let nrs           = fst <$> (Map.toList $ env^.env_world^._playerMap)

        buildTuple :: Int -> (Int, Maybe Client)
        buildTuple nr = (nr, view scl_client <$> Map.lookup nr (env^.env_clientMap))

        buildMsg :: Int -> Msg
        buildMsg nr   = SMsgWorld (buildTuple <$> nrs) nr (env^.env_isRunning)
    
    in
    (\nr -> (view scl_handle $ clientFromNr nr (env^.env_clientMap), buildMsg nr ))
    <$> (connectedClientsNr (env^.env_clientMap))
    


-- TODO: put this somewhere else
getPaddleBroadcast :: Int -> (NetworkTime, Float, Float) -> Env -> [(Handle,Msg)]
getPaddleBroadcast myNr tup env = 
    let msg            = MsgPaddle myNr tup
        handleFromNr n = view scl_handle $ clientFromNr n (env^.env_clientMap)
        nrs            = filter (/= myNr) $ connectedClientsNr (env^.env_clientMap)
    in zip (handleFromNr <$> nrs) (repeat msg)

-- broadcast the last ball in the ball list
getBallBroadcast :: Env -> [(Handle,Msg)]
getBallBroadcast env =
    --TODO take created balls directly
    let ball = last $ env^.env_world^._balls
        msg = SMsgBall 
                (ball^.Ball._referenceTime)
                (M.mkTuple3 $ ball^._position)
                (M.mkTuple3 $ ball^._direction)
                (M.mkTuple3 $ ball^._acceleration)
                (ball^.Ball._speed)
                (ball^.Ball._size)
    in
    (\nr -> (view scl_handle $ clientFromNr nr (env^.env_clientMap), msg ))
    <$> (connectedClientsNr (env^.env_clientMap))
    


-------------------------------------------------------------------------------
-- connection handling --------------------------------------------------------
-------------------------------------------------------------------------------


-- listen on listenSocket and call forkClient for every incomming connection
forkListener :: MVar Env -> Socket -> MsgHandlerServer Env -> IO ThreadId
{-forkListener :: MVar Env -> Socket -> (Int -> MsgHandler Env) -> IO ThreadId-}
forkListener mEnv listenSock handler = forkIO $ forever $ do
    (sock, _) <- accept listenSock
    handle <- socketToHandle sock ReadWriteMode 
    hSetBuffering handle NoBuffering
    forkClient mEnv handle handler

-- if game is running close socket and return,
-- otherwise add client to game and fork handler
-- when the client disconnects the playermap entry is removed or killed, depending on isRunning
forkClient :: MVar Env -> Handle -> MsgHandlerServer Env -> IO ThreadId
forkClient mEnv handle handlerServer = forkIO $ do

    maybeMsg    <- getMsg handle
    
    -- will hold the nr of the new player if authentification was valid, otherwise Nothing
    maybeNextNr <- modifyMVar mEnv $ \env -> do
        case (maybeMsg, env^.env_isRunning) of

            -- if game is not running jet and CMsgHello was received, add playermap entry
            (Just (CMsgHello nick), False) -> do
                putStrLn "acceptNewClient"
                let sClient  = SClient handle $ Client nick 0 True
                let (pm, nr) = Player.add (Player.new) (env^.env_world^._playerMap)
                let cm       = addClient sClient nr (env^.env_clientMap) 
                let newEnv   = (env_world._playerMap .~ pm) $
                               (env_clientMap        .~ cm) env
                return (newEnv, Just nr)

            -- otherwise close socket
            _ -> do 
                putStrLn "declineNewClient"
                hClose handle 
                return (env, Nothing)

    -- if client was added, broadcast world, then start message-handler
    -- when the client disconnects, broadcast again
    case maybeNextNr of
        Just nextNr -> do
            _ <- forkIO $ do
                putStrLn "start getMsgAndHandle"
                
                -- let everyone know we've got a new client
                putMsgs . getWorldBroadcast =<< readMVar mEnv

                -- let the network handler take over
                let handler = handlerServer & _2 %~ (\f -> f nextNr)
                getMsgAndHandle mEnv handle handler

                -- drop client when connection is closed, broadcast
                modifyMVar_ mEnv $ execStateT $ do
                    isRunning <- use env_isRunning
                    if isRunning
                        then do env_clientMap        %= killClient   nextNr
                        else do env_clientMap        %= removeClient nextNr
                                env_world._playerMap %= remove       nextNr
                    liftIO . putMsgs =<< getWorldBroadcast <$> get
                hClose handle

                putStrLn "end getMsgAndHandle"
            return ()
        _ -> return ()



-- create listening socket and empty env
setupConnection :: IO (Socket, MVar Env)
setupConnection = 
    let getListenSocket :: IO Socket
        getListenSocket = do
            addrInfos <- getAddrInfo
                         (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                         Nothing 
                         (Just "1337")
            let serverAddr = head addrInfos
            listenSock <- socket (addrFamily serverAddr) Stream defaultProtocol
            setSocketOption listenSock ReuseAddr 1
            bindSocket listenSock (addrAddress serverAddr)
            listen listenSock 5
            return listenSock

    in do
    s <- getListenSocket
    e <- newMVar =<< initEnv
    return (s, e)


-------------------------------------------------------------------------------
-- most code ------------------------------------------------------------------
-------------------------------------------------------------------------------

-- this is mostly for debug purposes
forkSimpleKeyboardHandler :: MVar Env -> IO ThreadId
forkSimpleKeyboardHandler mEnv = forkIO . forever $ do
    line <- getLine
    modifyMVar_ mEnv $ execStateT $ case line of
        "toggle" -> do
            liftIO $ putStrLn "toggleIsRunning"
            env_isRunning %= not
            list <- getWorldBroadcast <$> get
            liftIO $ putMsgs list 
        "env" -> do
            join $ (liftIO . putStrLn . show) <$> get
            
        _ -> do
            liftIO $ putStrLn "unknown command"

    
-- start one game
start :: IO ()
start = withSocketsDo $ do
    putStrLn "startServer"
    (listenSock, mEnv) <- setupConnection
    _<- forkListener mEnv listenSock (handleMsgPre , handleMsgPure, handleMsgPost)
    _<- forkSimpleKeyboardHandler mEnv



    modifyMVar_ mEnv $ execStateT $ do
        let walls = (fst $ Wall.initArena 5 1 10)
        env_world._extraWalls .= walls

    _<- forkBallHandler mEnv

    -- run main loop
    _<- forever $ do 
            threadDelay (2 * 1000000)
            {-modifyMVar_ mEnv $ execStateT stepEnv-}


    putStrLn "reached the end"
    return ()



stepEnv :: StateT Env IO ()
stepEnv = do
    assign env_timer =<< liftIO . ioUpdate =<< use env_timer



forkBallHandler :: MVar Env -> IO ThreadId
forkBallHandler mEnv = forkIO $ forever $ do


    modifyMVar_ mEnv $ execStateT stepEnv
    env <- readMVar mEnv

    modifyMVar_ mEnv $ execStateT (env_world %= update (env^.env_timer))

    let walls = env^.env_world^._extraWalls
    let ball  = last (env^.env_world^._balls)

    let currentTime = getTime (env^.env_timer)
    let (intersectTime,(wall,wallIdx :: Int)) = intersectList ball (zip walls (zip walls [0..]))

    let reflectedBall = reflect wall intersectTime ball
    modifyMVar_ mEnv $ execStateT ((env_world._balls) %= (addBall reflectedBall))

    putMsgs . getBallBroadcast =<< readMVar mEnv

    threadDelay $ floor $ 1000000 * (intersectTime - currentTime)
    putStrLn $ "bounce at wall " ++ (show wallIdx)

    return ()
    
