{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

import Network
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.Time
import Data.List

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
     ,clientHandle   :: Handle
     ,clientNick     :: String
     ,clientLastPong :: UTCTime
    } deriving Show
instance Eq Client where
  a == b = (clientHandle a) == (clientHandle b)



-- various kinds of messages

class Message m where 
  prefix      :: m -> String
  body        :: m -> String
  sendMessage :: m -> Handle -> IO ()
  sendMessage x h = hPutStr h $ (prefix x) ++ (body x)

data Ping = Ping
instance Message Ping where
  prefix _ = "PING"
  body   _ = ""

data Text = Text String
instance Message Text where
  prefix _      = "TEXT"
  body (Text t) = t

-----------------------------------------

-- HELPER FUNCTIONS

-- a logger which may need to be extended
logger :: String -> IO ()
logger s = putStrLn $ "[[" ++ s ++ "]]"


clientFromHandle :: Handle -> MVar Env -> IO (Maybe Client)
clientFromHandle handle envar = do
  e <- readMVar envar
  return $ find (\c -> clientHandle c == handle) (envClients e)
   

---------------------------------------

-- is running forever listening for new clients
-- if a new connection is accepted, it is added to envar client array
-- and a new clientHandler is forked
listenForClients :: Socket -> MVar Env -> IO ()
listenForClients listenSock envar = forever $ do 
  (handle, _, _) <- accept listenSock
  x <- readMVar envar 
  case envAcceptNew x of
    False -> declineNewClient handle
    True  -> acceptNewClient  handle envar
  where 
    declineNewClient handle = do
      logger "client declined"
      hClose handle
    acceptNewClient handle envar = do
      _ <- forkIO $ handleClient handle envar
      e <- takeMVar envar
      now <- getCurrentTime
      let client = Client { clientId = getFreeId e
                           ,clientHandle = handle
                           ,clientNick = "nick" 
                           ,clientLastPong = now } in
        putMVar envar e { envClients = client : (envClients e) }
        >> logger ("client" ++ (show $ clientId client) ++ " accepted")

getFreeId :: Env -> Int
getFreeId e = let Just id = find (\x -> x `notElem` map (\c -> clientId c)(envClients e)) [0..] in id



-- this is forked for every client
handleClient :: Handle -> MVar Env -> IO ()
handleClient handle envar = do
  line <- catch (fmap Right $ hGetLine handle) exceptionHandler
  Just c <- clientFromHandle handle envar
  case line of
    Left s  -> disconnectClient >> logger ("client" ++ (show $ clientId c) ++ " disconnected")
    Right s -> handleMessage s handle envar >> handleClient handle envar
  return ()
  where
    exceptionHandler :: SomeException -> IO (Either String String)
    exceptionHandler ex = return $ Left $ "Caught exception: " ++ show ex
    disconnectClient :: IO ()
    disconnectClient = do
      Just c <- clientFromHandle handle envar
      e <- takeMVar envar
      putMVar envar e { envClients = delete c (envClients e) }


-- this is called for every message
handleMessage :: String -> Handle -> MVar Env -> IO ()
handleMessage message handle envar = do
   Just c <- clientFromHandle handle envar
   putStrLn $ "client" ++ (show $ clientId c) ++ " says: " ++ message


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



-- the server entry point
start :: IO ()
start = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  listenSock <- listenOn $ PortNumber 1337
  envar <- newMVar Env { envClients   = []
                        ,envAcceptNew = True }
  _ <- forkIO $ inputHandler envar
  listenForClients listenSock envar
  return ()


  
    



  {-forever $ do-}
    {-(handle,host,_port) <- accept listenSock-}
    {-hSetBuffering handle NoBuffering-}
    {-now <- getCurrentTime-}
    {-let conn = Conn { connHandle = newHandle handle-}
                    {-, connHostname = host-}
                    {-, connServerName = configHostname config-}
                    {-, connTime = now-}
                    {-}-}
    {-_ <- forkIO $ handleClient config handle envar conn-}
    {-return ()-}

main :: IO ()
main = start

{-main = catch (print $ 5 `div` 0) handler-}
  {-where-}
    {-handler :: SomeException -> IO ()-}
    {-handler ex = putStrLn $ "Caught exception: " ++ show ex-}


{-foo = do-}
  {-putStrLn "hallo"-}
  {-x <- isDone-}
  {-case x of-}
    {-True  -> return ()-}
    {-False -> foo-}
  {-where isDone = return True-}
