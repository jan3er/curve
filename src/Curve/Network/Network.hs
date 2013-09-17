{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Curve.Network.Network 
    {-( module Curve.Network.Types-}
    {-, sendMsg-}
    {-, sendMsgs-}
    {-[>, recvMsgs<]-}
    {-, recvMsgAndHandle-}
    {-[>, recvMsgAndHandle2<]-}
    {-) where-}
    where

import           System.IO
import           Control.Concurrent
import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Loops

import           Data.Aeson.Generic

import           Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as BLC

{-import           Network.Socket-}

import           Curve.Network.Types        


-------------------------------------------------------------------------------
-- receiving ------------------------------------------------------------------
-------------------------------------------------------------------------------

{-recvMsgAndHandle :: Show a => MVar a -> Socket -> MsgHandler a -> IO ()-}
{-recvMsgAndHandle mEnv sock handler = do-}
    {-maybeMsg <- recvMsg sock-}
    {-[>case maybeMsg of<]-}
    {-case trace ("=> incomming: " ++ show maybeMsg) maybeMsg of-}
        {-Nothing  ->   -}
            {---TODO check sClose somewhere-}
            {-[>sClose sock<]-}
            {-return ()-}
            
        {-Just msg -> do-}
            {-modifyMVar_ mEnv $ execStateT $ do-}
                {-handler^._1-}
                {-list <- StateT (return . runState ((handler^._2) msg))-}
                {-handler^._3-}
                {-liftIO $ sendMsgList list-}
                {-liftIO $ putStrLn $ "sending: " ++ (show list)-}
            {-[>print =<< readMVar mEnv<]-}
            {-recvMsgAndHandle mEnv sock handler-}

{-recvMsgAndHandle :: Show a => MVar a -> Socket -> MsgHandler a -> IO ()-}
{-recvMsgAndHandle mEnv sock handler = do-}
    {-msgs :: [Msg] <- fromJust <$> recvMsg' sock-}
    {-sequence_ $ (\msg -> putStrLn $ show msg) <$> msgs-}


{--- receive a message over socket-}
{--- return nothing if connection is dead-}
{--- return Just Message otherwise-}
{-recvMsg :: Socket -> IO (Maybe Msg)-}
{-recvMsg sock = do-}
  {-putStrLn "depricated!!!!!!!"-}
  {-line <- recv sock 10000-}
  {-putStrLn $ show line-}
  {-if B.null line-}
    {-then return Nothing-}
    {-else return $ Just (decodeMsg line)-}
  {-where-}
    {-decodeMsg line = -}
      {-case decode (BL.fromChunks [line]) :: Maybe Msg of-}
        {-Nothing -> MsgUnknown-}
        {-Just x  -> x-}


{-recvMsg' :: Socket -> IO (Maybe [Msg])-}
{-recvMsg' sock = do-}
    {-line <- recv sock 10000-}
    {-putStrLn $ show line-}
    {-if B.null line-}
        {-then do-}
        {-return Nothing-}

        {-else do-}
        {-let maybeMsgs = bsToMaybeMsg <$> B.split delimiter line-}
        {-sequence_ $ (\msg -> putStrLn $ show msg) <$> B.split delimiter line-}
        {-putStrLn $ show maybeMsgs-}
        {-return $ Just $ (maybe MsgUnknown id) <$> maybeMsgs-}

{-bsToMaybeMsg :: B.ByteString -> Maybe Msg-}
{-bsToMaybeMsg line = decode (BL.fromChunks [line])-}

-------------------------------------------------------------------------------
-- sending --------------------------------------------------------------------
-------------------------------------------------------------------------------

-- runs until connection is closed
-- handle messages with given handler
getMsgAndHandle :: Show a => MVar a -> Handle -> MsgHandler a -> IO ()
getMsgAndHandle mEnv hdl handler =
    
    -- process a message by calling the handler on it
    let handleMsg msg = do
            putStrLn "got a nice message!"
            modifyMVar_ mEnv $ execStateT $ do
                handler^._1
                msgs <- StateT (return . runState ((handler^._2) msg))
                handler^._3
                liftIO $ putMsgs msgs

        handleNoMsg = do
            putStrLn "could not decrypt msg"
    in do
    -- process messages as long as the connection is up
    whileM_ (not <$> hIsEOF hdl) (maybe handleNoMsg handleMsg <$> getMsg hdl)

    -- todo: close connection?
    return ()

putMsgs :: [(Handle, Msg)] -> IO ()
putMsgs = mapM_ (\(h,m) -> putMsg h m)

putMsg :: Handle -> Msg -> IO ()
putMsg hdl msg = do
    putMsg' hdl $ trace ("=> outgoing: " ++ (show msg)) msg

getMsg :: Handle -> IO (Maybe Msg)
getMsg hdl = do
    msg <- getMsg' hdl
    return $ trace ("=> incoming: " ++ (show msg)) msg

    
putMsg' :: Handle -> Msg -> IO ()
putMsg' hdl = hPutStrLn hdl . BLC.unpack . encode

getMsg' :: Handle -> IO (Maybe Msg)
getMsg' hdl = hGetLine hdl >>= return . decode . BLC.pack


---------------------------------------------------------- ---------------------------------------------------------- ---------------------------------------------------------- 



-------------------------------------------------------------------------------
-- handling -------------------------------------------------------------------
-------------------------------------------------------------------------------

-- runs until connection is closed
-- handle messages with given handler
{-recvMsgAndHandle :: Show a => MVar a -> Socket -> MsgHandler a -> IO ()-}
{-recvMsgAndHandle mEnv sock handler = -}
    {--- process a message by calling the handler on it-}
    {-let handleMsg msg = do-}
            {-modifyMVar_ mEnv $ execStateT $ do-}
                {-handler^._1-}
                {-list <- StateT (return . runState ((handler^._2) msg))-}
                {-handler^._3-}
                {-liftIO $ sendMsgs list-}
                {-liftIO $ putStrLn $ "sending: " ++ (show list)-}

        {-handleNoMsg = do-}
            {-putStrLn "could not decrypt msg"-}
    {-in do-}
    {--- this ioref holds incomplete messages-}
    {-ioRest <- newIORef B.empty-}
    {--- process messages as long as the connection is up-}
    {-whileJust_ (recvMsgs sock ioRest) (mapM_ (maybe handleNoMsg handleMsg))-}
    {--- todo: close connection-}
    {-return ()-}

        

-------------------------------------------------------------------------------
-- basic send and receive------------------------------------------------------
-------------------------------------------------------------------------------

-- send a message somewhere
{-sendMsg :: Msg -> Socket -> IO ()-}
{-sendMsg msg' sock = do-}
    {-let msg  = trace ("=> outgoing: " ++ (show msg')) msg' --debug-}
    {-let line = merge $ encodeMsg <$> [msg]-}
    {-_ <- send sock line-}
    {-return ()-}

{-sendMsgs :: [(Socket, Msg)] -> IO ()-}
{-sendMsgs list = sequence_ $ (\(s,m) -> sendMsg m s) <$> list-}

--------------------------------------

-- receive a list of messages
-- is blocking
-- if the connection is closed return Nothing
-- otherwise return a list of maybe decoded messages
{-recvMsgs :: Socket -> IORef B.ByteString -> IO (Maybe [Maybe Msg])-}
{-recvMsgs sock ioRest = do -}
    {-line'   <- recv sock 424242-}
    {-let line = trace ("=> incomming: " ++ (show line')) line' --debug-}

    {-oldRest <- readIORef ioRest-}
    {-if B.null line-}
        {-then do return $ Nothing-}
        {-else do let (splits, newRest) = split line oldRest-}
                {-writeIORef ioRest newRest-}
                {-return $ Just $ decodeMsg <$> splits-}


{--- todo: put the rest of the msg in the bytestring-}
{-recvFirstMsg :: Socket -> IO (Maybe Msg, IORef B.ByteString)-}
{-recvFirstMsg sock = do-}
    {--- this ioref holds incomplete messages-}
    {-[>ioRest <- newIORef B.empty<]-}

    {--- receive as much as possible-}
    {-line <- recv sock 424242-}
    {--- take only the first message-}
    {-(maybeMsg, rest) <- splitFirst line B.empty-}

    {-hdl <- socketToHandle sock ReadWriteMode-}
    {-hSetBuffering hdl NoBuffering-}
    {-hPutStrLn hdl "Hi!"-}

    {-case maybeMaybeMsgs of-}
        {-Just (Just msg : xs) -> do-}
            {-writeIORef ioRest $ encodeMsg (tail xs)-}
            {-return (msg, ioRest)-}
    
             


-------------------------------------------------------------------------------
-- decode and encode msgs------------------------------------------------------
-------------------------------------------------------------------------------

{--- a character that is (hopefully) never used by json encoding-}
{-delimiter :: Word8-}
{-delimiter = 0-}

{--- take a received line and the unevaluated rest of the previous receive-}
{--- and return the messages split at the delimiter plus the new rest-}
{-split :: B.ByteString -> B.ByteString -> ([B.ByteString], B.ByteString)-}
{-split line rest = -}
    {-let splits = B.split delimiter (rest `B.append` line)-}
    {-in if B.null (last splits)-}
        {-then (splits, B.empty)-}
        {-else (init splits, last splits)-}

{--- return only the first message, and put the rest in the second tuple-entry-}
{-splitFirst :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)-}
{-splitFirst line rest =-}
    {-let (splits, newRest) = split line rest-}
    {-in (head splits, (merge $ tail splits) `B.append` newRest)-}

{--- concat a list of bytestrings and insert a delimiter after every string-}
{-merge :: [B.ByteString] -> B.ByteString-}
{-merge xs = B.concat $ (\l -> B.snoc l delimiter) <$> xs-}

{------------------------}

{-decodeMsg :: B.ByteString -> Maybe Msg-}
{-decodeMsg line = decode $ BL.fromChunks [line]-}

{-encodeMsg :: Msg -> B.ByteString-}
{-encodeMsg msg = B.concat $ BL.toChunks $ encode msg-}
