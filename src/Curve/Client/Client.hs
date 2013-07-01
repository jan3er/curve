{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Client.Client where

import           System.IO
import           Control.Concurrent
import           Control.Monad
import           Data.Time
import           Data.List
import           Network.Socket
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT

import           Curve.Network.Network


start :: IO ()
start = do
  addrInfo <- getAddrInfo Nothing 
                        (Just "192.168.178.21") 
                        (Just "1344")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  now <- getCurrentTime
  sendMessage sock TimeMessage { mTIME = now }
  m <- recvMessage sock
  putStrLn $ show $ m
  putStrLn "client"
