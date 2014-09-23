-- | Tutorial02 from open-tutorials.org adapted and ported to Haskell
-- and GLUT.
module Main where

import Common.Shader (ShaderRequest (..), loadShaders)
import Common.Window (untilClosed)

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL hiding (scale, translate, perspective)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Math.GLMat
import qualified Graphics.Math.GLMat as GM

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

main :: IO ()
main = do
  -- Initialize GLFW
  True <- GLFW.init
  GLFW.windowHint $ GLFW.WindowHint'Samples 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  -- Open a window and create its OpenGL context
  w@(Just window) <- GLFW.createWindow 1024 768 "Tutorial03" Nothing Nothing
  GLFW.makeContextCurrent w

  -- Ensure we can capture the escape key being pressed below
  GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled

  -- Dark blue background
  clearColor $= Color4 0 0 0.4 0

  vertexArrayId <- genObjectName
  bindVertexArrayObject $= Just vertexArrayId

  -- Create and compile GLSL program from the shaders
  programId <-
    loadShaders [ ShaderRequest VertexShader
                                "./src/Tutorial02/SimpleVertexShader.vert"
                , ShaderRequest FragmentShader
                                "./src/Tutorial02/SimpleFragmentShader.frag" ]

  matrixId <- get $ uniformLocation programId "MVP"

  let triangle = [ Vertex3 (-1) (-1) 0
                 , Vertex3   1  (-1) 0
                 , Vertex3   0    1  0 ] :: [Vertex3 GLfloat]
      numVertices = length triangle
      vertexSize  = sizeOf (head triangle)

  vertexBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just vertexBuffer
  withArray triangle $ \ptr -> do
    let size = fromIntegral (numVertices * vertexSize)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  window `untilClosed` (\_ -> do
    -- Clear the screen
    clear [ ColorBuffer ]

    -- Use our shader
    currentProgram $= Just programId

    let p = perspective 45 (4/3) 0.1 100
        v = GM.lookAt (GM.GLVector 4 3 3) (GM.GLVector 0 0 0)
                      (GM.GLVector 0 1 0)
        m = identity
        mat = p >*< v >*< m
    uniformMatrixF matrixId mat

    -- 1rst attribute buffer : vertices
    let vPosition  = AttribLocation 0
        firstIndex = 0
    vertexAttribArray vPosition $= Enabled
    vertexAttribPointer vPosition $=
      (ToFloat,
       VertexArrayDescriptor 3 Float 0 (bufferOffset (firstIndex * vertexSize)))
    drawArrays Triangles 0 3
    vertexAttribArray vPosition $= Disabled

    -- Swap buffers
    GLFW.swapBuffers window )
