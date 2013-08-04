{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Game.Misc where

import           Control.Concurrent
import           Control.Applicative

modifyMVar' :: MVar a -> (a -> a) -> IO ()
modifyMVar' mvar f = f <$> takeMVar mvar >>= putMVar mvar
