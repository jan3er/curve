{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Server.Timer where

import Data.Time
import Control.Lens
import Control.Monad.State

import Curve.Game.Timer

----------------------------------------

data STimer = STimer
    -- the local time at the moment the server initialized its time
    { _referenceTime    :: UTCTime  
    -- the current local Time
    , _localCurrentTime :: UTCTime 
    } deriving Show
makeLenses ''STimer

----------------------------------------

-- get a brand new timer
new :: IO STimer
new  = do
    t <- getCurrentTime
    return $ STimer t t

-- update the internal time of the timer
ioUpdate :: STimer -> IO STimer
ioUpdate = execStateT $ do
    currentTime <- liftIO $ getCurrentTime
    localCurrentTime .= currentTime

-- get the game-time
instance Timer STimer where
    getTime timer = diffUTCTime (timer^.localCurrentTime) (timer^.referenceTime)
