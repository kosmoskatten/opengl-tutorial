-- | Tutorial02 from open-tutorials.org adapted and ported to Haskell
-- and GLUT.
module Main where

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.UI.GLUT
import System.Exit (exitSuccess)

import Common.Shader

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

display :: Descriptor -> DisplayCallback
display (Descriptor triangle firstIndex numVertices) = do
  -- Clear the screen
  clear [ ColorBuffer ]
  drawArrays Triangles firstIndex numVertices
  bindVertexArrayObject $= Just triangle  
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
   triangle <- genObjectName
   bindVertexArrayObject $= Just triangle
   
   -- Create and compile our GLSL program from the shaders
   program <- loadShaders 
              [ ShaderRequest VertexShader 
                              "./src/Tutorial02/SimpleVertexShader.vert"
              , ShaderRequest FragmentShader
                              "./src/Tutorial02/SimpleFragmentShader.frag" ]
   -- Use our shader
   currentProgram $= Just program
   
   let vertexBufferData = [
           Vertex3 (-1) (-1) 0
         , Vertex3   1  (-1) 0
         , Vertex3   0    1  0 ] :: [Vertex3 GLfloat]
       numVertices = length vertexBufferData
       vertexSize  = sizeOf (head vertexBufferData)
                          
   vertexBuffer <- genObjectName
   bindBuffer ArrayBuffer $= Just vertexBuffer
   withArray vertexBufferData $ \ptr -> do
     let size = fromIntegral (numVertices * vertexSize)
     bufferData ArrayBuffer $= (size, ptr, StaticDraw)     
   
   let firstIndex = 0
       vPosition  = AttribLocation 0
   vertexAttribPointer vPosition $= 
     (ToFloat, 
      VertexArrayDescriptor 1 Float 0 (bufferOffset (firstIndex * vertexSize)))
   vertexAttribArray vPosition $= Enabled   
   
   displayCallback $= display (Descriptor triangle (fromIntegral firstIndex)
                                                   (fromIntegral numVertices))
   keyboardMouseCallback $= Just keyboard
   mainLoop
