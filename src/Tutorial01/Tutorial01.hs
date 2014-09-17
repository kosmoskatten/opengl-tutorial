-- | Tutorial01 from open-tutorials.org adapted ported to Haskell.
module Main where

import Common.Window (untilClosed)
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
  True <- GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  -- Open a window and create its OpenGL context
  w@(Just window) <- GLFW.createWindow 1024 768 "Tutorial 01" Nothing Nothing
  GLFW.makeContextCurrent w

  -- Ensure we can capture the escape key being pressed below
  GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled

  -- Dark blue background
  clearColor $= Color4 0 0 0.4 0

  window `untilClosed` (\_ -> do
    clear [ ColorBuffer ]
    GLFW.swapBuffers window)

  GLFW.terminate


