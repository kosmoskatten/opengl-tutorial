module Main where

import Common.Shader (ShaderRequest (..), loadShaders)
import Common.Window (untilClosed)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Math.GLMat
import Graphics.Math.GLMat as GM
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW

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
  w@(Just window) <- GLFW.createWindow 1024 768 "Tutorial04" Nothing Nothing
  GLFW.makeContextCurrent w

  -- Ensure we can capture the escape key beeing pressed below
  GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled

  -- Dark blue background
  clearColor $= Color4 0 0 0.4 0

  -- Enable depth test
  depthMask $= Enabled
  -- Accept fragment if it's closer to the camera than the former one
  depthFunc $= Just Less

  vertexArrayId <- genObjectName
  bindVertexArrayObject $= Just vertexArrayId

  -- Create and compile our GLSL program from the shaders
  programId <- loadShaders [ ShaderRequest VertexShader
                                           "./src/Tutorial04/Transform.vert"
                           , ShaderRequest FragmentShader
                                           "./src/Tutorial04/Color.frag"
                           ]

  -- Get a handle for our "MVP" uniform
  matrixId <- get $ uniformLocation programId "MVP"

  -- Projection matrix: 45 dgr field of view, 4:3 ratio, display
  -- range: 0.1 unit to 100 units
  let projection = GM.perspective 45 (4 / 3) 0.1 100 :: GM.GLMatrix GLfloat
      -- Camera matrix
      view       = GM.lookAt (GM.GLVector 4 3 (-3))
                             (GM.GLVector 0 0 0)
                             (GM.GLVector 0 1 0)
      -- Model matrix: an identity matrix (model will be at origin)
      model      = GM.identity
      -- Our ModelViewProjection: multiplication of our 3 matrices
      mvp        = projection >*< view >*< model

  -- Our vertices for the cube.
  let cube = [ Vertex3 (-1) (-1) (-1)
             , Vertex3 (-1) (-1)   1
             , Vertex3 (-1)   1    1
               
             , Vertex3   1    1  (-1)
             , Vertex3 (-1) (-1) (-1)
             , Vertex3 (-1)   1  (-1)

             , Vertex3   1  (-1)   1
             , Vertex3 (-1) (-1) (-1)
             , Vertex3   1  (-1) (-1)

             , Vertex3   1    1  (-1)
             , Vertex3   1  (-1) (-1)
             , Vertex3 (-1) (-1) (-1)

             , Vertex3 (-1) (-1) (-1)
             , Vertex3 (-1)   1    1
             , Vertex3 (-1)   1  (-1)

             , Vertex3   1  (-1)   1
             , Vertex3 (-1) (-1)   1
             , Vertex3 (-1) (-1) (-1)

             , Vertex3 (-1)   1    1
             , Vertex3 (-1) (-1)   1
             , Vertex3   1  (-1)   1

             , Vertex3   1    1    1
             , Vertex3   1  (-1) (-1)
             , Vertex3   1    1  (-1)

             , Vertex3   1  (-1) (-1)
             , Vertex3   1    1    1
             , Vertex3   1  (-1)   1

             , Vertex3   1    1    1
             , Vertex3   1    1  (-1)
             , Vertex3 (-1)   1  (-1)

             , Vertex3   1    1    1
             , Vertex3 (-1)   1  (-1)
             , Vertex3 (-1)   1    1

             , Vertex3   1    1    1
             , Vertex3 (-1)   1    1
             , Vertex3   1  (-1)   1
             ] :: [Vertex3 GLfloat]
      numVertices = length cube
      vertexSize  = sizeOf $ head cube

  vertexBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just vertexBuffer
  withArray cube $ \ptr -> do
    let size = fromIntegral (numVertices * vertexSize)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  window `untilClosed` (\_ -> do
    -- Clear the screen
    clear [ ColorBuffer, DepthBuffer ]

    -- Use our shader
    currentProgram $= Just programId

    -- Send our transformations to the currently bound shader
    GM.uniformMatrixF matrixId mvp

    -- 1rst attribute buffer: vertices
    let vPosition = AttribLocation 0
    vertexAttribArray vPosition $= Enabled
    bindBuffer ArrayBuffer $= Just vertexBuffer
    vertexAttribPointer vPosition $=
      (ToFloat,
       VertexArrayDescriptor 3 Float 0 (bufferOffset (0 * vertexSize)))
    drawArrays Triangles 0 (fromIntegral numVertices)

    vertexAttribArray vPosition $= Disabled

    -- Swap buffers
    GLFW.swapBuffers window )

  -- Cleanup VBO and shader
  deleteObjectName vertexBuffer
  deleteObjectName programId
  deleteObjectName vertexArrayId

  -- Close OpenGL
  GLFW.terminate
