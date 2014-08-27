module Common.Shader 
       ( ShaderRequest (..)
       , loadShaders
       ) where

import Control.Exception (bracketOnError)
import Control.Monad ((<=<), unless)
import qualified Data.ByteString as BS
import Graphics.Rendering.OpenGL

data ShaderRequest = ShaderRequest !ShaderType !FilePath
  deriving Show
           
loadShaders :: [ ShaderRequest ] -> IO Program
loadShaders xs =
  createProgram `bracketOnError` deleteObjectName $ \program -> do
    mapM_ (attachShader program <=< compile) xs
    check linkProgram linkStatus programInfoLog "link" program
    return program

compile :: ShaderRequest -> IO Shader
compile (ShaderRequest shType path) =
  createShader shType `bracketOnError` deleteObjectName $ \shader -> do
    source <- BS.readFile path
    shaderSourceBS shader $= source
    check compileShader compileStatus shaderInfoLog "compile" shader
    return shader

check :: (a -> IO ()) 
      -> (a -> GettableStateVar Bool)
      -> (a -> GettableStateVar String)
      -> String
      -> a
      -> IO ()
check action getStatus getInfoLog prefix object = do
  action object
  ok <- get $ getStatus object
  unless ok $ do
    cause <- get $ getInfoLog object
    fail $ prefix ++ " log:" ++ cause
