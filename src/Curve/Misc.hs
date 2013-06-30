{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Misc where

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


-- a logger which may need to be extended
logger :: String -> IO ()
logger s = putStrLn $ "[[" ++ s ++ "]]"

-- get the envars client entry for a given socket
clientFromSocket :: Socket -> MVar Env -> IO (Maybe Client)
clientFromSocket sock envar = do
  e <- readMVar envar
  return $ find (\c -> clientSocket c == sock) (envClients e)

-- convert bytestreams
toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks
toLazy :: B.ByteString -> BL.ByteString
toLazy s = BL.fromChunks [s]
   

