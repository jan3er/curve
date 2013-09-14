{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Client.Types where 

import qualified Data.Map as Map
import           Network.Socket
import           Control.Lens
import           Control.Applicative

import qualified Graphics.Rendering.OpenGL as GL

import           Curve.Network.Types
import           Curve.Game.Player

import qualified Curve.Game.Ball as Ball
import           Curve.Game.Ball (Ball)

import qualified Curve.Game.Wall as Wall
import           Curve.Game.Wall (Wall)

import qualified Curve.Client.Timer as Timer
import           Curve.Client.Timer (Timer)

----------------------------------------

type PlayerMap = Map.Map Int (Player, Maybe Client)


data Window = Window
    { _window_mousePos   :: GL.Position
    , _window_size       :: GL.Size
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
    , _env_ball          :: Ball
    } deriving Show
makeLenses ''Env

----------------------------------

initEnv :: Socket -> IO Env
initEnv sock = do
    let window = Window (GL.Position 0 0) (GL.Size 10 10)
    Env <$> pure Map.empty
        <*> pure sock
        <*> pure (-1)
        <*> pure False
        <*> Timer.init
        <*> pure window
        <*> pure Ball.init
