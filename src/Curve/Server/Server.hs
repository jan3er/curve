{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Server.Server where

import System.IO
import Control.Lens
import Control.Concurrent
import Control.Monad
import Control.Monad.State
{-import Control.Monad.Trans.Maybe-}
import Control.Monad.Trans.Error
import Control.Applicative
{-import Data.Maybe-}
{-import Data.Time-}
{-import Debug.Trace-}
{-import Data.List-}
{-import Data.Tuple-}
{-import qualified Data.Map.Lazy as Map-}
{-import Data.List-}

import Network.Socket

import Curve.Server.Env
import Curve.Server.ClientMap
import Curve.Server.Timer as Timer
{-import Curve.Game.Player as Player-}
import Curve.Game.Wall as Wall
import Curve.Game.World as World
import Curve.Game.Ball as Ball
{-import qualified Curve.Game.Math as M-}

import Curve.Game.Utils
import Curve.Game.Network
import Curve.Game.Timer

-------------------------------------------------------------------------------
-- IO message handlers --------------------------------------------------------
-------------------------------------------------------------------------------

handleMsgPre :: MsgHandlerPre Env
handleMsgPre = assign env_timer =<< liftIO . Timer.ioUpdate =<< use env_timer

handleMsgPost :: MsgHandlerPost Env
handleMsgPost = return ()

-------------------------------------------------------------------------------
-- pure message handler -------------------------------------------------------
-------------------------------------------------------------------------------


handleToClient :: Handle -> [ClientHandle] -> Client
handleToClient handle list =
    let f clientHandle = clientHandle^.clh_handle == handle
    in (head $ filter f list)^.clh_client --TODO

-- this is called for every incomming message
handleMsgPure :: MsgHandlerPure Env
handleMsgPure handle msg = do
    case msg of

        MsgPaddle _ (t, x, y) -> do 
            {-getPaddleBroadcast nr (t,x,y) <$> get-}
            return []

        CMsgHello nick -> do
            error "asdasdfasdf asdfasdf. not to be called"
            return []

        MsgTime _ -> do 
            msgOut <- MsgTime . getTime <$> use env_timer
            return [(handle, msgOut)]

        _ -> error "Server.handleMsg: no handler for this message"


{--- TODO: put this somewhere else-}
{-getWorldBroadcast :: Env -> [(Handle,Msg)]-}
{-getWorldBroadcast env = -}
    {-let nrs           = fst <$> (Map.toList $ env^.env_world^._playerMap)-}

        {-buildTuple :: Int -> (Int, Maybe Client)-}
        {-buildTuple nr = (nr, view scl_client <$> Map.lookup nr (env^.env_clientMap))-}

        {-buildMsg :: Int -> Msg-}
        {-buildMsg nr   = SMsgWorld (buildTuple <$> nrs) nr (env^.env_isRunning)-}
    
    {-in-}
    {-(\nr -> (view scl_handle $ clientFromNr nr (env^.env_clientMap), buildMsg nr ))-}
    {-<$> (connectedClientsNr (env^.env_clientMap))-}
    


{--- TODO: put this somewhere else-}
{-getPaddleBroadcast :: Int -> (NetworkTime, Float, Float) -> Env -> [(Handle,Msg)]-}
{-getPaddleBroadcast myNr tup env = -}
    {-let msg            = MsgPaddle myNr tup-}
        {-handleFromNr n = view scl_handle $ clientFromNr n (env^.env_clientMap)-}
        {-nrs            = filter (/= myNr) $ connectedClientsNr (env^.env_clientMap)-}
    {-in zip (handleFromNr <$> nrs) (repeat msg)-}

{--- broadcast the last ball in the ball list-}
{-getBallBroadcast :: Env -> [(Handle,Msg)]-}
{-getBallBroadcast env =-}
    {---TODO take created balls directly-}
    {-let ball = last $ env^.env_world^._balls-}
        {-msg = SMsgBall -}
                {-(ball^.Ball._referenceTime)-}
                {-(M.mkTuple3 $ ball^._position)-}
                {-(M.mkTuple3 $ ball^._direction)-}
                {-(M.mkTuple3 $ ball^._acceleration)-}
                {-(ball^.Ball._speed)-}
                {-(ball^.Ball._size)-}
    {-in-}
    {-(\nr -> (view scl_handle $ clientFromNr nr (env^.env_clientMap), msg ))-}
    {-<$> (connectedClientsNr (env^.env_clientMap))-}
    

-------------------------------------------------------------------------------
-- functions with env ---------------------------------------------------------
-------------------------------------------------------------------------------

getSMsgClients :: Env -> [(Handle, Msg)]
getSMsgClients env =
    let clientHandles = env^.env_clients
        makeMsg clientHandle idx = 
            ( clientHandle^.clh_handle
            , SMsgClients 
              { _SMsgClients_index   = idx 
              , _SMsgClients_clients = ((view clh_client) <$> clientHandles)  }
            )
    in zipWith makeMsg clientHandles [0..]


addClient :: String -> Handle -> State Env ()
addClient nick handle = do
    let client       = Client
                       { _cl_nick     = nick
                       , _cl_lastMsg  = 0
                       , _cl_isAlive  = True
                       , _cl_playerId = 42 } --TODOb
    let clientHandle = ClientHandle
                       { _clh_handle = handle
                       , _clh_client = client }
    env_clients %= (clientHandle:)

-------------------------------------------------------------------------------
-- connection handling --------------------------------------------------------
-------------------------------------------------------------------------------


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


-- listen on listenSocket and call forkClient for every incomming connection
listenForClients :: MVar Env -> Socket -> MsgHandler Env -> IO ()
listenForClients mEnv listenSock handler = forever $ do
    (sock, _) <- accept listenSock
    handle <- socketToHandle sock ReadWriteMode 
    hSetBuffering handle NoBuffering
    forkIO $ do
        putStrLn "client connected"
        result <- runErrorT $ acceptClient mEnv handle handler
        case result of
            Right () -> return ()
            Left e   -> putStrLn e
        putStrLn "client disconnected"
        

-- if game is running close socket and return,
-- otherwise add client to game and fork handler
-- when the client disconnects the playermap entry is removed or killed, depending on isRunning
acceptClient :: MVar Env -> Handle -> MsgHandler Env -> ErrorT String IO ()
acceptClient mEnv handle handler = do

    -- the first message is expected to be of kind CMsgHello
    msg  <- liftIO $ getMsg handle
    nick <- case msg of
        Just (CMsgHello n) -> return n
        _                  -> throwError "first message was no CMsgHello"

    -- the game is expected to not be running
    msgs <- modifyMVarErrorT mEnv $ do
        isRunning <- use env_isRunning
        if isRunning
            then do 
                lift (addClient nick handle)
                getSMsgClients <$> get
            else do 
                throwError "trying to add client while game is already running"

    liftIO $ putMsgs msgs
    liftIO $ getMsgAndHandle mEnv handle handler
    -- TODO delete client again once the connection died
    liftIO $ hClose handle

    
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- this is mostly for debug purposes
simpleKeyboardHandler :: MVar Env -> IO ()
simpleKeyboardHandler mEnv = forever $ do
    line <- getLine
    modifyMVar_ mEnv $ execStateT $ case line of
        "toggle" -> do
            liftIO $ putStrLn "toggleIsRunning"
            env_isRunning %= not
            --TODOa
            {-list <- getWorldBroadcast <$> get-}
            {-liftIO $ putMsgs list -}
        "env" -> do
            join $ (liftIO . putStrLn . show) <$> get
            
        _ -> do
            liftIO $ putStrLn "unknown command"

    
-- start one game
start :: IO ()
start = withSocketsDo $ do
    putStrLn "startServer"
    (listenSock, mEnv) <- setupConnection
    _<- forkIO $ listenForClients mEnv listenSock (MsgHandler handleMsgPre handleMsgPure handleMsgPost)
    _<- forkIO $ simpleKeyboardHandler mEnv



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

    --TODOa
    {-putMsgs . getBallBroadcast =<< readMVar mEnv-}

    threadDelay $ floor $ 1000000 * (intersectTime - currentTime)
    putStrLn $ "bounce at wall " ++ (show wallIdx)

    return ()
    
