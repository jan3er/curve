{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable #-}

import System.IO
import Control.Concurrent
import Control.Monad
import Control.Exception
import Control.Applicative

import Data.Functor
import Data.Time
import Data.List
import Data.Aeson
import Data.Typeable
import Data.Data
import qualified Data.Text as T
import qualified Data.Aeson.Generic as Generic
import qualified Data.ByteString.Lazy as B
{-import qualified Data.ByteString.Lazy.Internal as B-}

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

{-import Control.Applicative-}
{-import Control.Concurrent-}
{-import Control.Concurrent.Delay-}
{-import Control.Monad-}
{-import Control.Monad.Fix-}
{-import Control.Monad.Reader-}
{-import qualified Data.Map as M-}
{-import Data.Time-}
{-import Network-}
{-import Network.IRC-}
{-import System.IO-}
{-import System.IO.UTF8 as UTF8-}


-----------------------------------------

-- will hold the enviornment state
data Env = Env {
   envClients   :: [Client]
  ,envAcceptNew :: Bool
} deriving Show

data Client = Client {
      clientId       :: Int
     ,clientSocket   :: Socket
     ,clientNick     :: String
     ,clientLastPong :: UTCTime
    } deriving Show
instance Eq Client where
  a == b = (clientId a) == (clientId b)



-- various kinds of messages

{-class Message m where -}
  {-prefix      :: m -> String-}
  {-body        :: m -> String-}
  {-sendMessage :: Handle -> m -> IO ()-}
  {-sendMessage h x = hPutStr h $ (prefix x) ++ " " ++ (body x)-}

{-data Ping = Ping-}
{-instance Message Ping where-}
  {-prefix _ = "PING"-}
  {-body   _ = ""-}

{-class Message m where-}
  {-decodeFoo :: B.ByteString -> Maybe m-}
  {-decodeFoo message = Generic.decode message :: Maybe m-}

data TimeMessage = TimeMessage {
  timeMessageTime :: UTCTime
} deriving (Data,Typeable,Show)
{-instance Message TimeMessage-}

data TextMessage = TextMessage {
  textMessagetext :: String
} deriving (Data,Typeable,Show)
{-instance Message TextMessage-}


data Person = Person
     { personName :: String
     , personAge  :: Int
     } deriving (Data,Typeable,Show)
-----------------------------------------

-- HELPER FUNCTIONS

-- a logger which may need to be extended
logger :: String -> IO ()
logger s = putStrLn $ "[[" ++ s ++ "]]"

clientFromSocket :: Socket -> MVar Env -> IO (Maybe Client)
clientFromSocket sock envar = do
  e <- readMVar envar
  return $ find (\c -> clientSocket c == sock) (envClients e)
   

---------------------------------------


-- the server entry point
start :: IO ()
start = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  envar <- newMVar Env { envClients   = []
                        ,envAcceptNew = True }
  _ <- forkIO $ inputHandler envar

  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just "1339")
  let serveraddr = head addrinfos
  listenSock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket listenSock (addrAddress serveraddr)
  listen listenSock 5
  listenForClients listenSock envar
  return ()


-- is running forever listening for new clients
-- if a new connection is accepted, it is added to envar client array
-- and a new clientHandler is forked
listenForClients :: Socket -> MVar Env -> IO ()
listenForClients listenSock envar = forever $ do 
  (conn, _) <- accept listenSock
  x <- readMVar envar 
  case envAcceptNew x of
    False -> declineNewClient conn
    True  -> acceptNewClient  conn envar
  where 
    declineNewClient conn = do
      logger "client declined"
      sClose conn
    acceptNewClient conn envar = do
      e <- takeMVar envar
      now <- getCurrentTime
      let client = Client {clientId       = getFreeId e,
                           clientSocket   = conn,
                           clientNick     = "nick", 
                           clientLastPong = now }
      putMVar envar e { envClients = client : (envClients e) }
      logger ("client" ++ (show $ clientId client) ++ " accepted")
      _ <- forkIO $ handleClient conn envar
      return ()

getFreeId :: Env -> Int
getFreeId e = let Just id = find (\x -> x `notElem` map (\c -> clientId c)(envClients e)) [0..] in id


-- this is forked for every client
handleClient :: Socket -> MVar Env -> IO ()
handleClient sock envar = do
    message <- recv sock 100000 
    Just c <- clientFromSocket sock envar
    let lazy = B.fromChunks [message] :: B.ByteString
    let stripped = lazy
    if B.null lazy
      then do disconnectClient
              logger ("client" ++ (show $ clientId c) ++ " disconnected")
      else do handleMessage sock envar lazy
              handleClient sock envar
    where
    disconnectClient :: IO ()
    disconnectClient = do
      Just c <- clientFromSocket sock envar
      e <- takeMVar envar
      putMVar envar e { envClients = delete c (envClients e) }
      sClose sock

  {-Just c <- clientFromSocket sock envar-}
  {-case line of-}
    {-Left _  -> disconnectClient >> logger ("client" ++ (show $ clientId c) ++ " disconnected")-}
    {-Right s -> handleMessage s handle envar >> handleClient handle envar-}
  {-return ()-}
  {-where-}
    {-exceptionHandler :: SomeException -> IO (Either String String)-}
    {-exceptionHandler ex = return $ Left $ "Caught exception: " ++ show ex-}
    {-disconnectClient :: IO ()-}
    {-disconnectClient = do-}
      {-Just c <- clientFromSocket handle envar-}
      {-e <- takeMVar envar-}
      {-putMVar envar e { envClients = delete c (envClients e) }-}


-- this is called for every message
handleMessage :: Socket -> MVar Env -> B.ByteString -> IO ()
handleMessage sock envar message = do
  putStrLn $ show message
  {-let decoded = (Generic.decode message :: Maybe TextMessage,-}
                 {-Generic.decode message :: Maybe TimeMessage)-}
  {-printMessage $ fst decoded-}
  {-printMessage $ snd decoded-}
  {-where-}
    {-printMessage :: Maybe (Message m)-> IO ()-}
    {-printMessage d = case d of-}
      {-Just (TimeMessage m) -> putStrLn $ show m-}
      {-Just (TextMessage m) -> putStrLn $ show m-}
      {-_                    -> putStrLn "could not match"-}

  {-now <- getCurrentTime-}
  {-let t = TimeMessage { timeMessageTime = now }-}
  {-let tjson = Generic.encode t-}
  {-putStrLn $ show tjson-}

  {-Just c <- clientFromSocket sock envar-}
  {-putStrLn $ "client" ++ (show $ clientId c) ++ " says: " ++ (show message)-}
  {-case words message of-}
    {-"TIME" : body -> handleTimeMessage body-}
    {-_             -> logger "unknown packet"-}
  {-where-}
    {-handleTimeMessage _ = do-}
      {-now <- getCurrentTime-}
      {-sendMessage handle $ TimeMessage (show now)-}
    {-handleNickMessage_ = do -}
      {-e <- readMVar envar-}
      {-Just c <- clientFromHandle handle e-}

-- handles input from keyboard
inputHandler :: MVar Env -> IO ()
inputHandler envar = forever $ do
  line <- getLine
  case line of
    "toggle"  -> putStrLn "toggling acceptNew" >> toggleAcceptNew
    "envar"   -> readMVar envar >>= putStrLn . show
    _         -> putStrLn "unknown"
  where 
    toggleAcceptNew  = do
    x <- takeMVar envar
    putMVar envar x { envAcceptNew = not $ envAcceptNew x }




main :: IO ()
main = start
