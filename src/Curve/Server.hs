{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Server where

import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops
import           Control.Exception
import           Control.Applicative

import           Data.Functor
import           Data.Time
import           Data.List
import           Data.Aeson.Generic
import           Data.Typeable
import           Data.Data
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Curve.Types
import Curve.Misc



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
  envar <- newMVar Env { envClients   = []
                        ,envAcceptNew = True }
  _ <- forkIO $ inputHandler envar

  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just "1341")
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
      where 
        getFreeId :: Env -> Int
        getFreeId e = let Just id = find (\x -> x `notElem` map (\c -> clientId c)(envClients e)) [0..] in id



-- this is forked for every client
handleClient :: Socket -> MVar Env -> IO ()
handleClient sock envar = do
    message <- recv sock 100000 
    Just c <- clientFromSocket sock envar
    if B.null message
      then do disconnectClient
              logger ("client" ++ (show $ clientId c) ++ " disconnected")
      else do handleMessage sock envar message
              handleClient sock envar
    where
    disconnectClient :: IO ()
    disconnectClient = do
      Just c <- clientFromSocket sock envar
      e <- takeMVar envar
      putMVar envar e { envClients = delete c (envClients e) }
      sClose sock



-- this is called for every message
handleMessage :: Socket -> MVar Env -> B.ByteString -> IO ()
handleMessage sock envar message = do
  case (decode (toLazy message) :: Maybe Message) of 
    Just m -> processMessage m
    Nothing -> logger $ "unknown packet: " ++ (show message)
  where 
    processMessage (TimeMessage _) = do
      now <- getCurrentTime
      let m = encode $ TimeMessage { mTIME = now }
      _ <- send sock (toStrict m)
      putStrLn "time!"

    processMessage (TextMessage _) = do
      let m = encode $ TextMessage { mTEXT = "yay" }
      _ <- send sock (toStrict m)
      putStrLn "text!"
