{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
module Curve.Server.Misc where

import           Control.Concurrent
import           Data.List
import           Network.Socket

import Curve.Server.Types

-- a logger which may need to be extended
logger :: String -> IO ()
logger s = putStrLn $ "[[ " ++ s ++ " ]]"
