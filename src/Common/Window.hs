module Common.Window
       ( untilClosed
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import qualified Graphics.UI.GLFW as GLFW

untilClosed :: GLFW.Window -> (GLFW.Window -> IO ()) -> IO ()
untilClosed = go
  where
    go :: GLFW.Window -> (GLFW.Window -> IO ()) -> IO ()
    go window action = do
      action window
      GLFW.pollEvents
      ifNotShouldClose window $ go window action

    ifNotShouldClose :: GLFW.Window -> IO () -> IO ()
    ifNotShouldClose window action = do
      flip unless action
        =<< (||) <$> (isPressed <$> GLFW.getKey window GLFW.Key'Escape)
                 <*> GLFW.windowShouldClose window

    isPressed :: GLFW.KeyState -> Bool
    isPressed state = state == GLFW.KeyState'Pressed
