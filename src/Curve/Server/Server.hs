{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Curve.Server.Server where

import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Control.Applicative
import           Debug.Trace
import           Data.Time
import           Data.Maybe
import           Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set      as Set
{-import           Data.List-}

import           Network.Socket

import           Control.Lens

import           Curve.Server.Types
import           Curve.Server.Misc
import           Curve.Network.Network

import           Curve.Game.Misc
import           Curve.Game.Types




-- handles input from keyboard
{-inputHandler :: MVar Env -> IO ()-}
{-inputHandler envar = forever $ do-}
  {-line <- getLine-}
  {-case line of-}
    {-"toggle"  -> putStrLn "toggling isRunning" >> toggleIsRunning-}
    {-"env"     -> readMVar envar >>= putStrLn . show-}
    {-_         -> putStrLn "unknown"-}
  {-where -}
    {-toggleIsRunning = do -}
      {-env <- takeMVar envar-}
      {-[>let modifiedEnv = modify env_isRunning not env <]-}
      {-let modifiedEnv = env & env_isRunning %~ not-}
      {-broadcastWorld modifiedEnv-}
      {-putMVar envar modifiedEnv-}



{--- creates a new clean env-}
{-newEnv :: IO Env-}
{-newEnv =-}
    {-Env <$> pure Map.empty-}
        {-<*> pure False-}
        {-<*> getCurrentTime-}
             

-- the server entry point
{-start :: IO ()-}
{-start = withSocketsDo $ do-}
  {-putStrLn "server started"-}
  {-hSetBuffering stdout LineBuffering-}
  {-envar <- newMVar =<< newEnv-}
  {-_ <- forkIO $ inputHandler envar-}
  {-addrinfos <- getAddrInfo-}
               {-(Just (defaultHints {addrFlags = [AI_PASSIVE]}))-}
               {-Nothing -}
               {-(Just "1337")-}
  {-let serveraddr = head addrinfos-}
  {-listenSock <- socket (addrFamily serveraddr) Stream defaultProtocol-}
  {-setSocketOption listenSock ReuseAddr 1-}
  {-bindSocket listenSock (addrAddress serveraddr)-}
  {-listen listenSock 5-}
  {-[>listenForClients listenSock envar<]-}
  {-return ()-}




-- is running forever listening for new clients
-- if a new connection is accepted, it is added to envar client array
-- and a new clientHandler is forked
{-listenForClients :: Socket -> MVar Env -> IO ()-}
{-listenForClients listenSock envar = forever $ do -}
  {-(conn, _) <- accept listenSock-}
  {-env <- readMVar envar -}
  {-case env^.env_isRunning  of-}
    {-True  -> logger "conection refused" >> sClose conn-}
    {-False -> forkIO (acceptNewClient conn) >> return ()-}
  {-where -}
    {-acceptNewClient sock = do -}
      {-putStrLn "connection accepted"-}
      {-[>sendMsg sock (CMsgHello "jan") -- expected here<]-}
      {-msg <- recvMsg sock-}
      {-case msg of-}
        {-Just (CMsgHello nick) -> do-}
          {---take envar and add client to env-}
          {-envOld <- takeMVar envar-}
          {-now <- getCurrentTime-}
          {-let (pm, nr) = addClient (envOld^.env_playerMap) sock nick now -}
          {-let env = envOld & Control.Lens.set env_playerMap pm-}
          {--- TODO:-}
          {-[>let env = envOld & env_playerMap ^~ pm <]-}
          {---broadcast changed world-}
          {-broadcastWorld env-}
          {---put envar back and start handler-}
          {-putMVar envar env-}
          {-handleClient envar nr-}
        {-_ -> putStrLn $ ( show msg )-}
      {-putStrLn "end of connection"-}



{-handleClient :: MVar Env -> Int -> IO ()-}
{-handleClient envar nr = do-}
  {-env <- readMVar envar-}
  {-let Just (_, Just client) = Map.lookup nr (env^.env_playerMap )-}
  {-msg <- recvMsg $ client^.scl_socket -}
  {-case msg of-}
    {---connection closed? -> kill or delete client-}
    {-Nothing -> do let f = if env^.env_isRunning then killClient else Map.delete-}
                  {-[>modifyMVar' envar $ modify env_playerMap (f nr)<]-}
                  {-modifyMVar' envar $ env_playerMap %~ (f nr)-}
                  {-withMVar    envar broadcastWorld-}

    {--- TODO-}
    {-Just m  -> do handleMsg envar nr m-}
                  {-handleClient envar nr-}


-- this is forked for every client
{-handleClient :: Socket -> MVar Env -> IO ()-}
{-handleClient sock envar = do-}
    {-message <- recvMessage sock-}
    {-Just c <- clientFromSocket sock envar-}
    {-case message of-}
      {-Just m  -> do handleMessage sock envar m-}
                    {-handleClient sock envar-}
      {-Nothing -> do env <- takeMVar envar-}
                    {-let pm = envPlayerMap env-}
                    {-let newPm = if envAcceptNew env-}
                      {-then -}
                      {-else -}
                    {-sClose sock-}
                    {-logger "connection closed"-}


-------------------------MESSAGE HANDLING---------------------------------

-- this is called for every incomming message
{-handleMsg :: MVar Env -> Int -> Msg -> IO ()-}
{-handleMsg envar nr msg = do-}
  {-putStrLn $ show msg-}
  {-case msg of-}
    {-MsgPaddle _ (_, x, y) -> do env <- takeMVar envar-}
                                {-putStrLn $ (show x) ++ " " ++ (show y)-}
                                {-broadcastPaddlePos env nr msg-}
                                {-putMVar envar env -}

    {-MsgTime _             -> do env <- readMVar envar-}
                                {-t <- getCurrentTime-}
                                {-let m = MsgTime $ diffUTCTime t (env^.env_startTime)-}
                                {-sendMsg m $ (clientByNr env nr)^.scl_socket-}

    {-_ -> do logger "received unknown message" >> (putStrLn . show) msg-}

{--- boadcast a SMsgWorld to all connected clients-}
{-broadcastWorld :: Env -> IO ()-}
{-broadcastWorld env =-}
  {-let msgClients = map-}
        {-[>(\(nr, (_, c)) -> (nr, maybe Nothing (Just . get scl_client) c))<]-}
        {-(\(nr, (_, c)) -> (nr, (view scl_client) <$> c))-}
        {-(Map.toList $ env^.env_playerMap )-}
  {-in do-}
  {-mapM_ (\nr -> sendMsg (msgWorld msgClients nr) ((clientByNr env nr)^.scl_socket )) -}
        {-(getConnectedClients env)-}
  {-where-}
    {-msgWorld msgClients nr = -}
      {-SMsgWorld { -}
        {-_SMsgWorld_clients   = msgClients,-}
        {-_SMsgWorld_clientNr  = nr,-}
        {-_SMsgWorld_isRunning = env^.env_isRunning  -}
      {-}-}

{--- boadcast paddlePos to all clients except for the one with ID=nr-}
{--- accept only MsgPaddle-}
{--- TODO make more clear which nr is taken-}
{-broadcastPaddlePos :: Env -> Int -> Msg -> IO ()-}
{-broadcastPaddlePos env nr (MsgPaddle _ pos) = -}
  {-let msgPaddlePos = MsgPaddle { _MsgPaddle_nr = nr, _MsgPaddle_pos = pos }-}
  {-in do-}
  {-mapM_ (\nr' -> sendMsg msgPaddlePos ((clientByNr env nr')^.scl_socket ))-}
        {-(filter (/= nr) $ getConnectedClients env)-}


{--- return nrs of all connected clients-}
{-getConnectedClients :: Env -> [Int]-}
{-getConnectedClients env = -}
  {-let isAlive (_, (_, c)) = maybe False (^.(scl_client.cl_isAlive)) c-}
  {-in  map fst $ filter isAlive (Map.toList $ env^.env_playerMap)-}

{--- returns the socket by nr-}
{-clientByNr :: Env -> Int -> SClient-}
{-clientByNr env nr = (fromJust . snd) (fromJust $ Map.lookup nr (env^.env_playerMap ))-}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- pure message handlers ------------------------------------------------------
-------------------------------------------------------------------------------

stupidHandler :: Int -> MsgHandler Env
stupidHandler _ _ = do
    return []
    

-- this is called for every incomming message
handleMsg :: Int -> MsgHandler Env
handleMsg nr msg = do
    case msg of

        MsgPaddle _ (t, x, y) -> do 
            getPaddleBroadcast nr (t,x,y) <$> get

        CMsgHello nick -> do
            -- TODO: is this correct?
            let updateName = fmap $ scl_client.cl_nick .~ nick
            modify $ env_playerMap %~ Map.alter (fmap $ _2 %~ updateName) nr
            getWorldBroadcast <$> get


        MsgTime _ -> do 
            {-t <- getCurrentTime-}
            -- TODO put current time in env
            let msgOut = MsgTime 0
            sock <- socketFromNr nr <$> get
            return [(sock, msgOut)]

        {-_ -> error "Server.handleMsg"-}
        

    {-_ -> do logger "received unknown message" >> (putStrLn . show) msg-}


getWorldBroadcast :: Env -> [(Socket,Msg)]
getWorldBroadcast env = 
    let clients = map
            (_2 %~ \(_, c) -> view scl_client <$> c)
            (Map.toList $ env^.env_playerMap )
        msg nr = SMsgWorld clients nr (env^.env_isRunning)
    
    in map
    {-in map-}
        (\nr -> (socketFromNr nr env, msg nr ))
        (getConnectedClients env)
    

getPaddleBroadcast :: Int -> (NominalDiffTime, Float, Float) -> Env -> [(Socket,Msg)]
getPaddleBroadcast nr tup env = 
    let msg = MsgPaddle nr tup
    in map
        (\ nr' -> (socketFromNr nr' env, msg))
        (filter (/= nr) $ getConnectedClients env)
        {-(getConnectedClients env)-}
        {-(traceShow (getConnectedClients env) (getConnectedClients env))-}


socketFromNr :: Int -> Env -> Socket
socketFromNr nr env = 
    let entry = Map.lookup nr $ env^.env_playerMap
        maybeSocket = view scl_socket <$> join (snd <$> entry)
    in maybe (error "Server.socketFromNr") id maybeSocket


-- return nrs of all connected clients
getConnectedClients :: Env -> [Int]
getConnectedClients =
    let isAlive  = maybe False (view $ scl_client.cl_isAlive) . view _2
    in Set.toList . Map.keysSet . Map.filter isAlive . view env_playerMap   
    {-\env -> traceShow (env^.env_playerMap) [0]-}


-------------------------------------------------------------------------------
-- most code ------------------------------------------------------------------
-------------------------------------------------------------------------------

{-handleMsg :: MsgHandler Env-}
{-handleMsg msg = do-}
    {-return []-}

forkClient :: MVar Env -> Socket -> (Int -> MsgHandler Env) -> IO ()
forkClient mEnv sock handler = do

    let player  = Player []
    let sClient = SClient sock 
                          $ Client "---" 0 True

    nextNr <- modifyMVar mEnv $ \env -> do
        let playerMap = env^.env_playerMap
        let nextNr = (\(Just x) -> x) $ find (\x -> x `notElem` map fst (Map.toList playerMap)) [0..]
        let newEnv = env & env_playerMap %~ Map.insert nextNr (player, Just sClient)
        return (newEnv, nextNr)

    {-env <- modifyMVar_ mEnv $ execStateT $ do-}
        {-playerMap <- use env_playerMap-}
        {-let nextNr = (\(Just x) -> x) $ find (\x -> x `notElem` map fst (Map.toList playerMap)) [0..]-}
        {-env_playerMap %= Map.insert nextNr (player, Just sClient)-}

    _<- forkIO $ recvMsgAndHandle mEnv sock (handler nextNr)
    return ()

-- create listening socket
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
                     <*> getCurrentTime
    in do
    s <- getListenSocket
    e <- newMVar =<< getEnv
    return (s, e)



-- start one game
start :: IO ()
start = withSocketsDo $ do
    putStrLn "startServer"
    (listenSock, mEnv) <- setupConnection

    _<- forever $ do
        -- add every new connection to Env
        (sock, _) <- accept listenSock
        forkClient mEnv sock handleMsg
        return ()
        

    _<- forever $ return ()

    return ()

        


{-is running forever listening for new clients-}
{-if a new connection is accepted, it is added to envar client array-}
{-and a new clientHandler is forked-}
{-listenForClients :: Socket -> MVar Env -> IO ()-}
{-listenForClients listenSock envar = forever $ do-}
  {-(conn, _) <- accept listenSock-}
  {-env <- readMVar envar-}
  {-case env^.env_isRunning  of-}
    {-True  -> logger "conection refused" >> sClose conn-}
    {-False -> forkIO (acceptNewClient conn) >> return ()-}
  {-where-}
    {-acceptNewClient sock = do-}
      {-putStrLn "connection accepted"-}
      {-[>sendMsg sock (CMsgHello "jan") -- expected here<]-}
      {-msg <- recvMsg sock-}
      {-case msg of-}
        {-Just (CMsgHello nick) -> do-}
          {---take envar and add client to env-}
          {-envOld <- takeMVar envar-}
          {-now <- getCurrentTime-}
          {-let (pm, nr) = addClient (envOld^.env_playerMap) sock nick now-}
          {-let env = envOld & Control.Lens.set env_playerMap pm-}
          {--- TODO:-}
          {-[>let env = envOld & env_playerMap ^~ pm <]-}
          {---broadcast changed world-}
          {-broadcastWorld env-}
          {---put envar back and start handler-}
          {-putMVar envar env-}
          {-handleClient envar nr-}
        {-_ -> putStrLn $ ( show msg )-}
      {-putStrLn "end of connection"-}

