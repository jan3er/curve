{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Curve.Game.Utils where

import Data.Tuple
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe

---------------------------------------------

modifyMVarStateT :: MVar s -> StateT s IO a -> IO a
modifyMVarStateT mvar s = modifyMVar mvar (fmap swap . (runStateT s))

modifyMVarState :: MVar s -> State s a -> IO a
modifyMVarState mvar s = modifyMVar mvar (return . swap . (runState s))

modifyMVarMaybeT :: MVar s -> MaybeT (State s) a -> MaybeT IO a
modifyMVarMaybeT mvar = MaybeT . modifyMVarState mvar . runMaybeT

modifyMVarErrorT :: MVar s -> ErrorT e (State s) a -> ErrorT e IO a
modifyMVarErrorT mvar = ErrorT . modifyMVarState mvar . runErrorT

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

liftState :: (Monad m) => State s a -> StateT s m a
liftState x = StateT (return . runState x)
