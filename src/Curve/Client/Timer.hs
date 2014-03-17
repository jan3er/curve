{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Curve.Client.Timer where
{-module Curve.Client.Timer-}
    {-( Timer-}
    {-, Curve.Client.Timer.init-}
    {-, getTime-}
    {-, serverUpdate-}
    {-, ioUpdate-}
    {-, NominalDiffTime-}
    {-) where-}

import System.IO
import Data.Time
import Control.Lens
import Control.Monad.State

import Curve.Game.Network
import Curve.Game.Message
import Curve.Game.Timer

----------------------------------------

data CTimer = CTimer
    { __referenceTime    :: UTCTime             -- the local time at the moment the server initialized its time
    , __localLastQuery   :: UTCTime             -- the local time of the last query
    , __localCurrentTime :: UTCTime             -- the current local Time
    , __waitForResp      :: Bool                -- is there an outstanding reply to a time request?
    , __handle           :: Handle              -- the handle connection to the "time server"
    } deriving Show
makeLenses ''CTimer

queryInterval :: Float
queryInterval = 2

----------------------------------------

-- get a brand new timer
initTimer :: Handle -> IO CTimer
initTimer handle = do
    putMessage handle (MessageTime 0)
    t <- getCurrentTime
    return $ CTimer 
        t
        t
        t
        False
        handle


-- update the timer with message from the server
serverUpdate:: Message -> CTimer -> CTimer
serverUpdate (MessageTime t) timer = 
    let mediumLocalTime = addUTCTime 
            (0.5 * diffUTCTime (timer^._localCurrentTime) (timer^._localLastQuery))
            (timer^._localLastQuery) 
        newReferenceTime =
            addUTCTime (-1*t) mediumLocalTime 
    in timer 
        { __referenceTime = newReferenceTime
        , __waitForResp   = False 
        } 
serverUpdate _ _ = error "Timer.update: wrong Message"


-- update the internal time of the timer and maybe get message to be sent to server
ioUpdate :: CTimer -> IO CTimer
ioUpdate = execStateT $ do
    currentTime <- liftIO $ getCurrentTime
    _localCurrentTime .= currentTime

    timer <- get
    let diff :: Float = realToFrac $ diffUTCTime (timer^._localCurrentTime) (timer^._localLastQuery)
    if ((diff >= queryInterval) && not (timer^._waitForResp))
        then do
            _waitForResp    .= True
            _localLastQuery .= currentTime
            liftIO $ putMessage (timer^._handle) (MessageTime 0)
            return ()
        else do
            return ()


-- get the game-time
instance Timer CTimer where
    getTime timer = diffUTCTime (timer^._localCurrentTime) (timer^._referenceTime)
