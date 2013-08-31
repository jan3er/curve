{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-module Curve.Client.Timer where-}
module Curve.Client.Timer
    ( Timer
    , initTime
    , getTime
    , serverUpdate
    , ioUpdate
    , GameTime
    ) where

import           Data.Time
import           Control.Lens
import           Control.Monad.State

import           Curve.Network.Types


type GameTime = NominalDiffTime

data Timer = Timer
    { _timer_referenceTime    :: UTCTime             -- the local time at the moment the server initialized its time
    , _timer_localLastQuery   :: UTCTime             -- the local time of the last query
    , _timer_localCurrentTime :: UTCTime             -- the current local Time
    , _timer_waitForResp      :: Bool                -- is there an outstanding reply to a time request?
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
        t
        False


-- update the timer with message from the server
serverUpdate:: Msg -> Timer -> Timer
serverUpdate (MsgTime t) timer = 
    let mediumLocalTime = addUTCTime 
            (0.5 * diffUTCTime (timer^.timer_localCurrentTime) (timer^.timer_localLastQuery))
            (timer^.timer_localLastQuery) 
        newReferenceTime =
            addUTCTime (-1*t) mediumLocalTime 
    in timer 
        { _timer_referenceTime = newReferenceTime
        , _timer_waitForResp   = False 
        } 
serverUpdate _ _ = error "Timer.update: wrong Msg"


-- update the internal time of the timer and maybe get message to be sent to server
ioUpdate :: Timer -> IO (Maybe Msg, Timer)
ioUpdate = runStateT $ do
    currentTime <- liftIO $ getCurrentTime
    timer_localCurrentTime .= currentTime

    timer <- get
    let diff :: Float = realToFrac $ diffUTCTime (timer^.timer_localCurrentTime) (timer^.timer_localLastQuery)
    if ((diff >= queryInterval) && not (timer^.timer_waitForResp))
        then do
            timer_waitForResp    .= True
            timer_localLastQuery .= currentTime
            return $ Just (MsgTime 0)
        else do
            return Nothing


-- get the game-time
getTime :: Timer -> GameTime
getTime timer = diffUTCTime (timer^.timer_localCurrentTime) (timer^.timer_referenceTime)
