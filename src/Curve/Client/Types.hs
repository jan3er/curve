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

import qualified Curve.Client.Timer as Timer



----------------------------------------

type PlayerMap = Map.Map Int (Player, Maybe Client)


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
    , _env_timer         :: Timer.Timer
    , _env_window        :: Window
    } deriving Show
makeLenses ''Env

----------------------------------
