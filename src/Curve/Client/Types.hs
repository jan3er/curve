{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, DeriveDataTypeable, ExistentialQuantification, TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Client.Types where 

import           Data.Time
import qualified Data.Map as Map

import           Network.Socket

import           Control.Lens

import qualified Graphics.Rendering.OpenGL as GL

import           Curve.Network.Types
import           Curve.Game.Types



----------------------------------------

type PlayerMap = Map.Map Int (Player, Maybe Client)


data Timer = Timer
    { _timer_referenceTime :: UTCTime             -- the local time at the moment the server initialized its time
    , _timer_localTime     :: UTCTime             -- the current local Time; the only thing needed from "outside"
    , _timer_lastQuery     :: UTCTime             -- the local time of the last query
    , _timer_waitForResp   :: Bool                -- is there an outstanding reply to a time request?
    } deriving Show
makeLenses ''Timer
toGlobalTime :: Timer -> UTCTime -> NominalDiffTime
toGlobalTime timer t = diffUTCTime t (timer^.timer_referenceTime)
{-timeToFloat :: NominalDiffTime -> Float-}
{-timeToFloat = realToFrac-}


data Window = Window
    { _window_mousePos   :: GL.Position
    } deriving Show
makeLenses ''Window


-- holds the enviornments state
data Env = Env 
    { _env_playerMap     :: PlayerMap
    , _env_socket        :: Socket
    , _env_nr            :: Int
    , _env_isRunning     :: Bool
    , _env_timer         :: Timer
    , _env_window        :: Window
    } deriving Show
makeLenses ''Env

----------------------------------

-- time between two msgTime polls in seconds
{-queryOffset :: Int-}
{-queryOffset = 1-}
