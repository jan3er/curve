{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Curve.Client.Types where 

import qualified Data.Map as Map
import           System.IO
import           Control.Lens

import qualified Graphics.Rendering.OpenGL as GL

import           Curve.Game.Network

{-import qualified Curve.Game.Ball as Ball-}
{-import           Curve.Game.Ball (Ball)-}

{-import qualified Curve.Game.Wall as Wall-}
{-import           Curve.Game.Wall (Wall)-}

import Curve.Game.World
import Curve.Client.Timer

----------------------------------------

data Window = Window
    { _window_mousePos   :: GL.Position
    , _window_size       :: GL.Size
    } deriving Show
makeLenses ''Window


-- holds the enviornments state
data Env = Env 
    { _env_clients       :: [Client]
    , _env_world         :: World
    , _env_handle        :: Handle
    , _env_playerId      :: Int
    , _env_isRunning     :: Bool
    , _env_timer         :: CTimer
    , _env_window        :: Window
    } deriving Show
makeLenses ''Env

----------------------------------

initEnv :: Handle -> IO Env
initEnv handle = do
    timer <- initTimer handle
    let window = Window (GL.Position 0 0) (GL.Size 10 10)
    return $ Env 
        { _env_clients   = []
        , _env_world     = initEmptyWorld
        , _env_handle    = handle
        , _env_playerId  = (-1)
        , _env_isRunning = False
        , _env_timer     = timer
        , _env_window    = window }
