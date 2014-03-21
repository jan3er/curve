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
    { __referenceTime    :: UTCTime  
    -- the current local Time
    , __localCurrentTime :: UTCTime 
    } deriving Show
makeLenses ''STimer

----------------------------------------

-- get a brand new timer
initTimer :: IO STimer
initTimer = do
    t <- getCurrentTime
    return $ STimer t t

-- update the internal time of the timer
ioUpdate :: STimer -> IO STimer
ioUpdate = execStateT $ do
    currentTime <- liftIO $ getCurrentTime
    _localCurrentTime .= currentTime

instance Timer STimer where
    getTime timer = diffUTCTime (timer^._localCurrentTime) (timer^._referenceTime)
    setReferenceTime t = _referenceTime %~ addUTCTime t
