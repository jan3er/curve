{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Curve.Client.Render.Renderer where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

display :: IO ()
display = do
  clear [ ColorBuffer ]
  flush

{-mouseMotion :: Position -> IO ()-}
{-mouseMotion pos = do-}
  {-putStrLn $ show pos-}

{-keyboardMouse key state modifiers position = do-}
  {-putStrLn "event!"-}
