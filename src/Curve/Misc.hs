{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Misc where

import           Control.Concurrent
import           Data.List
import           Network.Socket

import Curve.Types

-- a logger which may need to be extended
logger :: String -> IO ()
logger s = putStrLn $ "[[ " ++ s ++ " ]]"

-- get the envars client entry for a given socket
clientFromSocket :: Socket -> MVar Env -> IO (Maybe Client)
clientFromSocket sock envar = do
  e <- readMVar envar
  return $ find (\c -> clientSocket c == sock) (envClients e)
