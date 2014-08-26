-- | Tutorial01 from open-tutorials.org adapted ported to Haskell and
-- GLUT.
module Main where

import Graphics.UI.GLUT
import System.Exit (exitSuccess)

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  flush

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitSuccess
keyboard _ _ _ _ = return ()

main :: IO ()
main = do
   (progName, _) <- getArgsAndInitialize
   initialDisplayMode $= [ RGBAMode ]
   initialWindowSize $= Size 1024 768
--   initialContextVersion $= (3, 3)
--   initialContextProfile $= [ CompatibilityProfile, CoreProfile ]
   _ <- createWindow progName
   
   clearColor $= Color4 0.0 0.0 0.4 0.0
   displayCallback $= display
   keyboardMouseCallback $= Just keyboard
   mainLoop
