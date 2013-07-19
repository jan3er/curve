{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Main where
import Curve.Client.Client (start)





import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main :: IO ()
main = start
 
{-main :: IO ()-}
{-main = do-}
  {-(progname, _) <- getArgsAndInitialize-}
  {-createWindow "Hello World"-}
  {-displayCallback $= display-}
  {-passiveMotionCallback $= Just mouseMotion-}
  {-mainLoop-}
 
display :: IO ()
display = do
  clear [ ColorBuffer ]
  flush

mouseMotion :: Position -> IO ()
mouseMotion pos = do
  putStrLn $ show pos

{-keyboardMouse key state modifiers position = do-}
  {-putStrLn "event!"-}
