{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Server.Server where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Control.Applicative
import           Debug.Trace
import           Data.Time
import           Data.Maybe
import           Data.List
import qualified Data.Map.Lazy as Map
{-import           Data.List-}

import           Network.Socket

import           Control.Lens

import           Curve.Server.Env
import           Curve.Server.PlayerMap
import qualified Curve.Server.Timer as Timer

import           Curve.Network.Network

----------------------------------------

type MsgHandlerServer a = (MsgHandlerPre a, (Int -> MsgHandlerPure a), MsgHandlerPost a)

-------------------------------------------------------------------------------
-- IO message handlers --------------------------------------------------------
-------------------------------------------------------------------------------

handleMsgPre :: MsgHandlerPre Env
handleMsgPre = do 
    timer <- (\t -> liftIO $ Timer.ioUpdate t) =<< use env_timer
    env_timer .= timer

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
            modify $ env_playerMap %~ Map.alter (fmap $ _2 %~ updateName) nr
            getWorldBroadcast <$> get


        MsgTime _ -> do 
            -- TODO put current time in env
            msgOut <- MsgTime . Timer.getTime <$> use env_timer
            sock <- view scl_socket . clientFromNr nr . view env_playerMap <$> get
            return [(sock, msgOut)]

        _ -> error "Server.handleMsg: no handler for this message"


getWorldBroadcast :: Env -> [(Socket,Msg)]
getWorldBroadcast env = 
    let clients = map
            (_2 %~ \(_, c) -> view scl_client <$> c)
            (Map.toList $ env^.env_playerMap )
        msg nr = SMsgWorld clients nr (env^.env_isRunning)
    
    in 
    (\nr -> (view scl_socket $ clientFromNr nr (env^.env_playerMap), msg nr ))
    <$> (connectedClientsNr (env^.env_playerMap))
    

getPaddleBroadcast :: Int -> (NominalDiffTime, Float, Float) -> Env -> [(Socket,Msg)]
getPaddleBroadcast nr tup env = 
    let msg = MsgPaddle nr tup
    in 
    (\ nr' -> (view scl_socket $ clientFromNr nr' (env^.env_playerMap), msg))
    <$> (filter (/= nr) $ connectedClientsNr (env^.env_playerMap))
        {-(getConnectedClients env)-}
        {-(traceShow (getConnectedClients env) (getConnectedClients env))-}


-------------------------------------------------------------------------------
-- connection handling --------------------------------------------------------
-------------------------------------------------------------------------------


-- listen on listenSocket and call forkClient for every incomming connection
forkListener :: MVar Env -> Socket -> MsgHandlerServer Env -> IO ThreadId
{-forkListener :: MVar Env -> Socket -> (Int -> MsgHandler Env) -> IO ThreadId-}
forkListener mEnv listenSock handler = forkIO . forever $ do
    (sock, _) <- accept listenSock
    forkClient mEnv sock handler

-- if game is running close socket and return,
-- otherwise add client to game and fork handler
-- when the client disconnects the playermap entry is removed or killed, depending on isRunning
forkClient :: MVar Env -> Socket -> MsgHandlerServer Env -> IO ThreadId
forkClient mEnv sock handlerServer = forkIO $ do

    maybeMsg <- recvMsg sock 
    maybeNextNr <- modifyMVar mEnv $ \env -> do
        case (maybeMsg, env^.env_isRunning) of

            -- if game is not running jet and CMsgHello was received, add playermap entry
            (Just (CMsgHello nick), False) -> do
                putStrLn "acceptNewClient"
                let sClient                = SClient sock $ Client nick 0 True
                let (newPlayerMap, nextNr) = addClient sClient (env^.env_playerMap)
                let newEnv = env & env_playerMap .~ newPlayerMap
                return (newEnv, Just nextNr)

            -- otherwise close socket
            _ -> do 
                putStrLn "declineNewClient"
                sClose sock
                return (env, Nothing)

    -- if client was added, broadcast world, then start message-handler
    -- when the client disconnects, broadcast again
    case maybeNextNr of
        Just nextNr -> do
            _ <- forkIO $ do
                putStrLn "fork recvMsgAndHandle"
                sendMsgList . getWorldBroadcast =<< readMVar mEnv

                let handler = handlerServer & _2 %~ (\f -> f nextNr)
                recvMsgAndHandle mEnv sock handler

                modifyMVar_ mEnv $ execStateT $ do
                    isRunning <- use env_isRunning
                    env_playerMap %= removeOrKillClient isRunning nextNr
                    liftIO . sendMsgList =<< getWorldBroadcast <$> get
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

        getEnv :: IO Env
        getEnv = Env <$> pure Map.empty
                     <*> pure False
                     <*> Timer.initTime
    in do
    s <- getListenSocket
    e <- newMVar =<< getEnv
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
            liftIO $ sendMsgList list 
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

    -- run main loop
    _<- forever $ do modifyMVar_ mEnv $ execStateT stepEnv

    putStrLn "reached the end"
    return ()



stepEnv :: StateT Env IO ()
stepEnv = do
    return ()


