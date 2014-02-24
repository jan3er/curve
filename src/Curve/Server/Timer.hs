{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.Timer where

import Data.Time
import Control.Lens
import Control.Monad.State

import Curve.Game.Network

----------------------------------------

data Timer = Timer
    -- the local time at the moment the server initialized its time
    { _referenceTime    :: UTCTime  
    -- the current local Time
    , _localCurrentTime :: UTCTime 
    } deriving Show
makeLenses ''Timer

----------------------------------------

-- get a brand new timer
new :: IO Timer
new  = do
    t <- getCurrentTime
    return $ Timer t t

-- update the internal time of the timer
ioUpdate :: Timer -> IO Timer
ioUpdate = execStateT $ do
    currentTime <- liftIO $ getCurrentTime
    localCurrentTime .= currentTime

-- get the game-time
getTime :: Timer -> NetworkTime
getTime timer = diffUTCTime (timer^.localCurrentTime) (timer^.referenceTime)
