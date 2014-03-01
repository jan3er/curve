{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-module Curve.Client.Timer where-}
module Curve.Client.Timer
    ( Timer
    , Curve.Client.Timer.init
    , getTime
    , serverUpdate
    , ioUpdate
    , NetworkTime
    ) where

import System.IO
import Data.Time
import Control.Lens
import Control.Monad.State

import Curve.Game.Network

----------------------------------------

data Timer = Timer
    { __referenceTime    :: UTCTime             -- the local time at the moment the server initialized its time
    , __localLastQuery   :: UTCTime             -- the local time of the last query
    , __localCurrentTime :: UTCTime             -- the current local Time
    , __waitForResp      :: Bool                -- is there an outstanding reply to a time request?
    , __handle           :: Handle              -- the handle connection to the "time server"
    } deriving Show
makeLenses ''Timer

queryInterval :: Float
queryInterval = 2

----------------------------------------

-- get a brand new timer
init :: Handle -> IO Timer
init handle = do
    putMsg handle (MsgTime 0)
    t <- getCurrentTime
    return $ Timer 
        t
        t
        t
        False
        handle


-- update the timer with message from the server
serverUpdate:: Msg -> Timer -> Timer
serverUpdate (MsgTime t) timer = 
    let mediumLocalTime = addUTCTime 
            (0.5 * diffUTCTime (timer^._localCurrentTime) (timer^._localLastQuery))
            (timer^._localLastQuery) 
        newReferenceTime =
            addUTCTime (-1*t) mediumLocalTime 
    in timer 
        { __referenceTime = newReferenceTime
        , __waitForResp   = False 
        } 
serverUpdate _ _ = error "Timer.update: wrong Msg"


-- update the internal time of the timer and maybe get message to be sent to server
ioUpdate :: Timer -> IO Timer
ioUpdate = execStateT $ do
    currentTime <- liftIO $ getCurrentTime
    _localCurrentTime .= currentTime

    timer <- get
    let diff :: Float = realToFrac $ diffUTCTime (timer^._localCurrentTime) (timer^._localLastQuery)
    if ((diff >= queryInterval) && not (timer^._waitForResp))
        then do
            _waitForResp    .= True
            _localLastQuery .= currentTime
            liftIO $ putMsg (timer^._handle) (MsgTime 0)
            return ()
        else do
            return ()


-- get the game-time
getTime :: Timer -> NetworkTime
getTime timer = diffUTCTime (timer^._localCurrentTime) (timer^._referenceTime)
