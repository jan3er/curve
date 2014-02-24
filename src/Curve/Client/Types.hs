{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Client.Types where 

import qualified Data.Map as Map
import           System.IO
import           Control.Lens

import qualified Graphics.Rendering.OpenGL as GL

import           Curve.Network.Network

{-import qualified Curve.Game.Ball as Ball-}
{-import           Curve.Game.Ball (Ball)-}

{-import qualified Curve.Game.Wall as Wall-}
{-import           Curve.Game.Wall (Wall)-}

import qualified Curve.Game.World as World
import           Curve.Game.World (World)

import qualified Curve.Client.Timer as Timer
import           Curve.Client.Timer (Timer)

----------------------------------------

type ClientMap = Map.Map Int Client

data Window = Window
    { _window_mousePos   :: GL.Position
    , _window_size       :: GL.Size
    } deriving Show
makeLenses ''Window


-- holds the enviornments state
data Env = Env 
    { _env_clientMap     :: ClientMap
    , _env_world         :: World
    , _env_handle        :: Handle
    , _env_nr            :: Int
    , _env_isRunning     :: Bool
    , _env_timer         :: Timer
    , _env_window        :: Window
    } deriving Show
makeLenses ''Env

----------------------------------

initEnv :: Handle -> IO Env
initEnv handle = do
    timer <- Timer.init handle
    let window = Window (GL.Position 0 0) (GL.Size 10 10)
    return $ Env Map.empty
                 World.new
                 handle
                 (-1)
                 False
                 timer
                 window
