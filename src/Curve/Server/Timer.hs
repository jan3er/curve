{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Server.Timer where

import           Data.Time
import           Control.Lens
import           Control.Monad.State

import           Curve.Network.Types


type GameTime = NominalDiffTime

data Timer = Timer
    { _timer_referenceTime    :: UTCTime             -- the local time at the moment the server initialized its time
    , _timer_localCurrentTime :: UTCTime             -- the current local Time
    } deriving Show
makeLenses ''Timer

queryInterval :: Float
queryInterval = 2

--------------------------------------------------------------------------------------

-- get a brand new timer
initTime :: IO Timer
initTime = do
    t <- getCurrentTime
    return $ Timer 
        t
        t

-- update the internal time of the timer
ioUpdate :: Timer -> IO Timer
ioUpdate = execStateT $ do
    currentTime <- liftIO $ getCurrentTime
    timer_localCurrentTime .= currentTime

-- get the game-time
getTime :: Timer -> GameTime
getTime timer = diffUTCTime (timer^.timer_localCurrentTime) (timer^.timer_referenceTime)
