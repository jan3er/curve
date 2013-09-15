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
    , GameTime
    ) where

{-import           Debug.Trace-}
import           Data.Time
import           Control.Lens
import           Control.Monad.State

import           Curve.Network.Types


type GameTime = NominalDiffTime

data Timer = Timer
    { _referenceTime    :: UTCTime             -- the local time at the moment the server initialized its time
    , _localLastQuery   :: UTCTime             -- the local time of the last query
    , _localCurrentTime :: UTCTime             -- the current local Time
    , _waitForResp      :: Bool                -- is there an outstanding reply to a time request?
    } deriving Show
makeLenses ''Timer

queryInterval :: Float
queryInterval = 2

--------------------------------------------------------------------------------------

-- get a brand new timer
init :: IO Timer
init = do
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
            (0.5 * diffUTCTime (timer^.localCurrentTime) (timer^.localLastQuery))
            (timer^.localLastQuery) 
        newReferenceTime =
            addUTCTime (-1*t) mediumLocalTime 
    in timer 
        { _referenceTime = newReferenceTime
        , _waitForResp   = False 
        } 
serverUpdate _ _ = error "Timer.update: wrong Msg"


-- update the internal time of the timer and maybe get message to be sent to server
ioUpdate :: Timer -> IO (Maybe Msg, Timer)
ioUpdate = runStateT $ do
    currentTime <- liftIO $ getCurrentTime
    localCurrentTime .= currentTime

    timer <- get
    let diff :: Float = realToFrac $ diffUTCTime (timer^.localCurrentTime) (timer^.localLastQuery)
    if ((diff >= queryInterval) && not (timer^.waitForResp))
        then do
            waitForResp    .= True
            localLastQuery .= currentTime
            return $ Just (MsgTime 0)
        else do
            return Nothing


-- get the game-time
getTime :: Timer -> GameTime
{-getTime timer = -}
    {-let t = diffUTCTime (timer^.localCurrentTime) (timer^.referenceTime)-}
    {-in trace (show t) t-}
getTime timer = diffUTCTime (timer^.localCurrentTime) (timer^.referenceTime)
