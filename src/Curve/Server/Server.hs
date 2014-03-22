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
import Curve.Server.ClientHandle
import Curve.Server.Timer as Timer
{-import Curve.Game.Player as Player-}
import Curve.Game.Wall as Wall
import Curve.Game.World as World
import Curve.Game.Ball as Ball
{-import qualified Curve.Game.Math as M-}

import Curve.Game.Utils
import Curve.Game.Network
import Curve.Game.Message
import Curve.Game.Timer

-------------------------------------------------------------------------------
-- IO message handlers --------------------------------------------------------
-------------------------------------------------------------------------------

handleMessagePre :: MessageHandlerPre Env
handleMessagePre = updateEnv

handleMessagePost :: MessageHandlerPost Env
handleMessagePost = return ()

-------------------------------------------------------------------------------
-- pure message handler -------------------------------------------------------
-------------------------------------------------------------------------------


handleToClient :: Handle -> [ClientHandle] -> Client
handleToClient handle list =
    let f clientHandle = clientHandle^.clh_handle == handle
    in (head $ filter f list)^.clh_client --TODO

-- this is called for every incomming message
handleMessagePure :: MessageHandlerPure Env
handleMessagePure handle message = do
    case message of

        {-MessagePaddle _ (t, x, y) -> do -}
            {-[>getPaddleBroadcast nr (t,x,y) <$> get<]-}
            {-return []-}

        {-CMessageHello nick -> do-}
            {-error "asdasdfasdf asdfasdf. not to be called"-}
            {-return []-}

        -- help the client sync its time
        MessageTime _ -> do
            msgOut <- (MessageTime . getTime) <$> use env_timer
            return [(handle, msgOut)]

        _ -> do 
            error $ "Server.handleMessagePure: no handler for this message: \n" ++ (show message)



{--- TODO: put this somewhere else-}
{-getWorldBroadcast :: Env -> [(Handle,Message)]-}
{-getWorldBroadcast env = -}
    {-let nrs           = fst <$> (Map.toList $ env^.env_world^._playerMap)-}

        {-buildTuple :: Int -> (Int, Maybe Client)-}
        {-buildTuple nr = (nr, view scl_client <$> Map.lookup nr (env^.env_clientMap))-}

        {-buildMessage :: Int -> Message-}
        {-buildMessage nr   = SMessageWorld (buildTuple <$> nrs) nr (env^.env_isRunning)-}
    
    {-in-}
    {-(\nr -> (view scl_handle $ clientFromNr nr (env^.env_clientMap), buildMessage nr ))-}
    {-<$> (connectedClientsNr (env^.env_clientMap))-}
    


{--- TODO: put this somewhere else-}
{-getPaddleBroadcast :: Int -> (NominalDiffTime, Float, Float) -> Env -> [(Handle,Message)]-}
{-getPaddleBroadcast myNr tup env = -}
    {-let msg            = MessagePaddle myNr tup-}
        {-handleFromNr n = view scl_handle $ clientFromNr n (env^.env_clientMap)-}
        {-nrs            = filter (/= myNr) $ connectedClientsNr (env^.env_clientMap)-}
    {-in zip (handleFromNr <$> nrs) (repeat msg)-}

{--- broadcast the last ball in the ball list-}
{-getBallBroadcast :: Env -> [(Handle,Message)]-}
{-getBallBroadcast env =-}
    {---TODO take created balls directly-}
    {-let ball = last $ env^.env_world^._balls-}
        {-msg = SMessageBall -}
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
-- message creation -----------------------------------------------------------
-------------------------------------------------------------------------------

-- TODO make some higherorder function
-- blub :: (handle -> idx -> env -> message) -> env -> [(handle, message)]

createSMessageClients :: Env -> [(Handle, Message)]
createSMessageClients env =
    let clientHandles = env^.env_clients
        makeMessage clientHandle idx = 
            ( clientHandle^.clh_handle
            , SMessageClients 
              { _SMessageClients_index   = idx 
              , _SMessageClients_clients = ((view clh_client) <$> clientHandles)  }
            )
    in zipWith makeMessage clientHandles [0..]


createSMessageRoundStart :: Env -> [(Handle, Message)]
createSMessageRoundStart env =
    let handles   = view clh_handle <$> env^.env_clients
        {-startBall = currentBall $ env^.env_world^._balls -}
        {-noPlayers = length      $ env^.env_world^._players-}
        {-message   = SMessageRoundStart-}
            {-{ _SMessageRoundStart_numberOfPlayers = noPlayers-}
            {-, _SMessageRoundStart_startBall       = startBall }-}
        message = SMessageRoundStart (env^.env_world) (getTime $ env^.env_timer)
    in zip handles (repeat message)
        
createSMessageBall :: Env -> [(Handle, Message)]
createSMessageBall env = 
    let handles   = view clh_handle <$> env^.env_clients
        message = SMessageBall $ last (env^.env_world^._balls)
    in zip handles (repeat message)

-------------------------------------------------------------------------------
-- functions with env ---------------------------------------------------------
-------------------------------------------------------------------------------


addClient :: String -> Handle -> State Env ()
addClient nick handle = do
    let client       = Client
                       { _cl_nick     = nick
                       , _cl_lastMessage  = 0
                       , _cl_isAlive  = True
                       , _cl_playerId = (-1) } --TODOb
    let clientHandle = ClientHandle
                       { _clh_handle = handle
                       , _clh_client = client }
    env_clients %= (clientHandle:)

removeClient :: Handle -> State Env ()
removeClient handle = do
    env_clients %= filter ((/=) handle . view clh_handle)


updateEnv :: StateT Env IO ()
updateEnv = do 
    timer <- liftIO . ioUpdate =<< use env_timer
    env_timer .= timer
    env_world %= updateWorld timer

    {-modifyMVar_ mEnv $ execStateT (env_world %= updateWorld (env^.env_timer))-}
    {-update :: (Timer t) => t -> World -> World-}


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
listenForClients :: MVar Env -> Socket -> MessageHandler Env -> IO ()
listenForClients mEnv listenSock handler = forever $ do
    (sock, _) <- accept listenSock
    handle <- socketToHandle sock ReadWriteMode 
    hSetBuffering handle NoBuffering
    forkIO $ do
        putStrLn "client connected"
        result <- runErrorT $ acceptClient mEnv handle handler
        case result of
            Right () -> return ()
            Left e   -> putStrLn ("ERROR: " ++ e)
        putStrLn "client disconnected"
        

-- add a client to the client list and call the message handler
-- fails if the game is already running
acceptClient :: MVar Env -> Handle -> MessageHandler Env -> ErrorT String IO ()
acceptClient mEnv handle handler = do

    -- the first message is expected to be of kind CMessageHello
    msg  <- liftIO $ getMessage handle
    nick <- case msg of
        Just (CMessageHello n) -> return n
        _                  -> throwError "first message was no CMessageHello"

    -- the game is expected not to be running when a client joins
    msgsJoin <- modifyMVarErrorT mEnv $ do
        isRunning <- use env_isRunning
        if isRunning
            then do 
                throwError "trying to add client while game is already running"
            else do 
                lift (addClient nick handle)
                createSMessageClients <$> get

    -- broadcast the changed client list
    liftIO $ putMessages msgsJoin

    -- let the handler take over until the connection dies
    liftIO $ getMessageAndHandle mEnv handle handler

    -- remove the client afterwards
    msgsLeave <- liftIO $ modifyMVarState mEnv $ do
        removeClient handle
        createSMessageClients <$> get
    liftIO $ putMessages msgsLeave

    
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


startRound :: MVar Env -> IO ()
startRound mEnv = do
    putStrLn "=> startRound"

    -- update the env
    modifyMVarStateT mEnv updateEnv

    -- add some arbitrary number of players
    modifyMVarState mEnv $ do
        env_world .= initWorld 5

    -- inform the clients that the game starts
    putMessages =<< withMVar mEnv (return . createSMessageRoundStart)
    
    -- reset the timer
    modifyMVarState mEnv $ do
         env_timer %= \timer -> setReferenceTime (getTime timer) timer

    _<- forkIO $ ballHandler mEnv

    putStrLn "=> end startRound" 


{-endRound-}


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- this is mostly for debug purposes
simpleKeyboardHandler :: MVar Env -> IO ()
simpleKeyboardHandler mEnv = forever $ do
    line <- getLine
    modifyMVarStateT mEnv $ case line of
        {-"toggle" -> do-}
            {-liftIO $ putStrLn "toggleIsRunning"-}
            {-env_isRunning %= not-}
            --TODOa
            {-list <- getWorldBroadcast <$> get-}
            {-liftIO $ putMessages list -}
        "env" -> do
            liftIO . putStrLn =<<  show <$> get
        "s" -> do
            _<- liftIO $ forkIO $ startRound mEnv
            return ()
        _ -> do
            liftIO $ putStrLn "unknown command"

    
-- start one game
start :: IO ()
start = withSocketsDo $ do
    putStrLn "startServer"
    (listenSock, mEnv) <- setupConnection
    _<- forkIO $ listenForClients mEnv listenSock 
                 (MessageHandler handleMessagePre handleMessagePure handleMessagePost)
    _<- forkIO $ simpleKeyboardHandler mEnv


    {-_<- forkBallHandler mEnv-}

    -- run main loop
    _<- forever $ do 
        threadDelay (2 * 1000000)
        return ()
        {-modifyMVar_ mEnv $ execStateT stepEnv-}


    putStrLn "reached the end"
    return ()



{-stepEnv :: StateT Env IO ()-}
{-stepEnv = do-}
    {-updateTimer-}


ballHandler :: MVar Env -> IO ()
ballHandler mEnv = forever $ do


    -- update timer
    modifyMVar_ mEnv $ execStateT updateEnv

    
    env <- readMVar mEnv
    let currentTime = getTime (env^.env_timer)
    let (isCertain, eitherBall) = mainWorldMethod (env^.env_world)

    putStrLn $ "isCertain " ++ show isCertain
    putStrLn $ "eitherBall " ++ show eitherBall

    let ball = case eitherBall of
            Left _  -> error "asdfasdlhasfdkjhaf"
            Right b -> b

    let intersectTime = ball^.Ball._referenceTime

    modifyMVar_ mEnv $ execStateT ((env_world._balls) %= (addBall ball))

    putMessages . createSMessageBall =<< readMVar mEnv

    -- sleep until next intersection
    threadDelay $ floor $ 1000000 * (intersectTime - currentTime)

    return ()

