{-# LANGUAGE RecordWildCards #-}
module Main (
    main
) where

import System.Environment (getArgs)

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.Raw ( glUniformMatrix4fv
                                     , glUniform1f
                                     , glEnableVertexAttribArray
                                     , glBindBuffer
                                     , gl_ARRAY_BUFFER
                                     , glVertexAttribPointer
                                     , gl_FLOAT
                                     , glDeleteBuffers
                                     , glDisableVertexAttribArray
                                     , glDeleteVertexArrays
                                     , glGenVertexArrays
                                     , glBindVertexArray
                                     , glBufferData
                                     , glGenBuffers
                                     , gl_STATIC_DRAW
                                     )
import Graphics.GLUtil

import Foreign
import Data.ByteString.Char8 (pack)
import Data.IORef (IORef, newIORef)
import Data.Vec (Mat44, identity)

import FloretSphere
import Geometry ( Point3f (Point3f)
                , normalized
                , cross
                , times
                , lookAtMatrix
                , orthoMatrix
                , multMat
                , vec3)
import ListUtil


data Action = Action (IO Action, CameraState -> CameraState)

type Mat44Float = Mat44 GLfloat

data CameraState = CameraState { theta :: GLdouble
                               , phi :: GLdouble
                               , mouseX :: GLint
                               , mouseY :: GLint
                               , projMat :: IORef Mat44Float
                               , viewMat :: IORef Mat44Float
                               }


-- what we render
usePolyhedron :: Polyhedron
usePolyhedron = polyhedrons !! (-3 + length polyhedrons)


-- data buffers


-- vertex position buffer
vertexBufferData :: [GLfloat]
vertexBufferData =
  map realToFrac $ facesToFlatTriangles (vertice usePolyhedron) (faces usePolyhedron)


-- attribute buffer to distinguish edge points from face center points
centerBufferData :: [GLfloat]
centerBufferData =
  map realToFrac $ facesToCenterFlags $ faces usePolyhedron


-- indice linked to the vertex buffer
indexBufferData :: [GLuint]
indexBufferData =
  map fromIntegral $ facesToFlatIndice $ faces usePolyhedron


-- how many to draw
indexCount :: GLint
indexCount = fromIntegral $ length indexBufferData


-- GL Stuff
data GLIDs = GLIDs { prog :: Program
                   , indice :: BufferObject
                   , vertexBufferId :: GLuint
                   , vertexAttrib :: GLuint
                   , centerBufferId :: GLuint
                   , centerAttrib :: GLuint
                   , vertexArrayId :: GLuint
                   }


initGL :: IO GLIDs
initGL = do
  GL.depthFunc $= Just GL.Less
  GL.blend $= Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.shadeModel $= GL.Smooth
--  ambient (Light 0) $= Color4 0.1 0.1 0.1 (1 :: GLfloat)
--  diffuse (Light 0) $= Color4 0.9 0.9 0.9 (1 :: GLfloat)
--  light (Light 0) $= Enabled

  GL.polygonMode $= (Fill, Fill)

  GL.cullFace $= Just Back
--  GL.cullFace $= Nothing

  -- make shaders & data
  prog <- loadProgram "polyhedra.vert" "polyhedra.frag"
  (AttribLocation vertexAttrib) <- get (attribLocation prog "position")
  (AttribLocation centerAttrib) <- get (attribLocation prog "a_barycentric")
  indice <- makeBuffer ElementArrayBuffer indexBufferData

  vertexArrayId <- withNewPtr (glGenVertexArrays 1)
  glBindVertexArray vertexArrayId

  vertexBufferId <- fillNewBuffer vertexBufferData
  centerBufferId <- fillNewBuffer centerBufferData

  return GLIDs{..}


cleanUpGLStuff :: GLIDs -> IO ()
cleanUpGLStuff GLIDs{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  with centerBufferId $ glDeleteBuffers 1
  with vertexArrayId $ glDeleteVertexArrays 1


-- rendering code


bindGeometry :: GLIDs -> IO ()
bindGeometry GLIDs{..} = do bindFloatBufferToAttrib 3 vertexBufferId vertexAttrib
                            bindFloatBufferToAttrib 1 centerBufferId centerAttrib


unbindGeometry :: GLIDs -> IO ()
unbindGeometry GLIDs{..} = do glDisableVertexAttribArray vertexAttrib
                              glDisableVertexAttribArray centerAttrib


render :: CameraState -> GLIDs -> IO ()
render state glids@GLIDs{..} = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  currentProgram $= Just prog

  -- bind view projection matrix
  (UniformLocation mvpLoc) <- get $ uniformLocation prog "u_worldView"
  projection <- get $ projMat state
  view <- get $ viewMat state
  with (multMat projection view) $ glUniformMatrix4fv mvpLoc 1 (fromBool True) . castPtr

  -- bind uniform colors
  colLoc <- get $ uniformLocation prog "u_color"
  bColLoc <- get $ uniformLocation prog "u_borderColor"
  uniform colLoc $= Color4 1 1 1 (1 :: GLfloat)
  uniform bColLoc $= Color4 0.4 0.4 0.4 (1 :: GLfloat)

  -- bind attributes (position and center flags)
  bindGeometry glids
  -- bind indice
  bindBuffer ElementArrayBuffer $= Just (indice)

  drawElements Triangles indexCount UnsignedInt offset0

  unbindGeometry glids

  GLFW.swapBuffers


-- shader creation, low level opengl code


withNewPtr f = alloca (\p -> f p >> peek p)


fillNewBuffer :: [GLfloat] -> IO GLuint
fillNewBuffer list = do
  bufId <- withNewPtr (glGenBuffers 1)
  glBindBuffer gl_ARRAY_BUFFER bufId
  withArrayLen list $ \length ptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral (length * sizeOf (undefined :: GLfloat)))
                 (ptr :: Ptr GLfloat) gl_STATIC_DRAW
  return bufId


bindFloatBufferToAttrib :: GLint -> GLuint -> GLuint -> IO ()
bindFloatBufferToAttrib components bufId attribLoc = do
  glEnableVertexAttribArray attribLoc
  glBindBuffer gl_ARRAY_BUFFER bufId
  glVertexAttribPointer attribLoc -- attribute location in the shader
                        components -- 3 components per vertex
                        gl_FLOAT -- coordinates type
                        (fromBool False) -- normalize?
                        (fromIntegral $ sizeOf (undefined::GLfloat) * fromIntegral components) -- stride
                        nullPtr -- vertex buffer offset


loadProgram :: String -> String -> IO Program
loadProgram vertShader fragShader = do
  shaders <- mapM (uncurry loadShader)
    [ (VertexShader, vertShader)
    , (FragmentShader, fragShader)
    ]
  prog <- createProgram
  mapM_ (attachShader prog) shaders

  putStrLn "Linking program"
  linkProgram prog
  linked <- get (linkStatus prog)
  putStr "Linked: "
  putStr $ show linked

  validateProgram prog
  valid <- get (validateStatus prog)
  putStr ", valid: "
  putStr $ show valid

  infoLog <- get (programInfoLog prog)
  putStr ", info log: "
  putStrLn infoLog

  mapM_ (detachShader prog) shaders
  releaseShaderCompiler

  return prog


-- camera functions


defaultCameraState :: IORef Mat44Float -> IORef Mat44Float -> CameraState
defaultCameraState projMat viewMat = CameraState { theta = 2.5*pi/4.0
                                                 , phi = pi/3.0
                                                 , mouseX = 0
                                                 , mouseY = 0
                                                 , projMat = projMat
                                                 , viewMat = viewMat
                                                 }


camPos :: CameraState -> (GLdouble, GLdouble, GLdouble)
camPos cam = (50 * sin (phi cam) * cos (theta cam), 50 * cos (phi cam), 50 * sin (phi cam) * sin (theta cam))


camToPoint3f :: CameraState -> Point3f
camToPoint3f cam = Point3f (realToFrac x) (realToFrac y) (realToFrac z)
  where (x,y,z) = camPos cam


-- input handling / UI


resize :: IORef Mat44Float -> GLFW.WindowSizeCallback
resize projMatRef size@(Size w h) = do
  let hh = if h < 0 then 1 else h
  let aspect = (fromIntegral w) / (fromIntegral hh)
  GL.viewport   $= (Position 0 0, size)

  let far = 200
  let near = 10
  let top = 4
  let right = top*aspect
  let left = -right
  let bottom = -top

  projMatRef $= orthoMatrix (left :: GLfloat)
                            right
                            bottom
                            top
                            near
                            far

  return ()


limitAngle :: (Floating a, Ord a) => a -> a
limitAngle angle =
  if angle < 0.01
    then 0.01
    else if angle > pi
      then pi
      else angle


onClick :: GLint -> GLint -> CameraState -> CameraState
onClick newX newY state =
  state { mouseX = newX
        , mouseY = newY
        }


applyMouseMove :: GLint -> GLint -> CameraState -> CameraState
applyMouseMove newX newY state =
  state { mouseX = newX
        , mouseY = newY
        , theta = theta0 + (fromIntegral diffX) / 100.0
        , phi = limitAngle $ phi0 + (fromIntegral diffY) / 100.0
        }
  where
    diffX = newX - mouseX state
    diffY = -newY + mouseY state
    theta0 = theta state
    phi0 = phi state


waitForPress :: IO Action
waitForPress = do
  b <- GLFW.getMouseButton GLFW.ButtonLeft
  case b of
    GLFW.Release -> return (Action (waitForPress, id))
    GLFW.Press   -> do
      -- when left mouse button is pressed,
      -- switch to waitForRelease action.
      (GL.Position x y) <- GL.get GLFW.mousePos
      return (Action (waitForRelease, onClick x y))


waitForRelease :: IO Action
waitForRelease = do
  -- keep track of mouse movement while waiting for button
  -- release
  (GL.Position x y) <- GL.get GLFW.mousePos
  b <- GLFW.getMouseButton GLFW.ButtonLeft
  case b of
    -- when button is released, switch back back to
    -- waitForPress action
    GLFW.Release -> return (Action (waitForPress, applyMouseMove x y))
    GLFW.Press   -> return (Action (waitForRelease, applyMouseMove x y))


loop :: IO Action -> CameraState -> Float -> GLIDs -> IO ()
loop action state lastTime glids = do
  -- dt
  nowD <- get time
  let now = realToFrac nowD
  let dt = realToFrac $ now - lastTime

  -- game
  Action (action', stateUpdater) <- action

  let newState = stateUpdater state

  let (Point3f px py pz) = camToPoint3f state
  viewMat state $= lookAtMatrix (vec3 (realToFrac px) (realToFrac py) (realToFrac pz))
                                (vec3 0 0 0)
                                (vec3 0 1 0)

  render newState glids

  -- exit if window closed or Esc pressed
  esc <- GLFW.getKey GLFW.ESC
  q <- GLFW.getKey 'Q'
  open <- GLFW.getParam GLFW.Opened
  if open && esc /= GLFW.Press && q /= GLFW.Press
    then loop action' newState now glids
    else return ()


main :: IO ()
main = do

--  putStrLn $ show $

  args <- getArgs
  let fullScreenRequest = (<) 0 $ length $ filter (\s -> s == "--f") args

  -- init
  projMat <- newIORef identity
  viewMat <- newIORef identity
  let state = defaultCameraState projMat viewMat

  -- initialize
  GLFW.initialize
  fullscreenMode <- get GLFW.desktopMode

  let bits = [ GLFW.DisplayRGBBits 8 8 8
             , GLFW.DisplayAlphaBits 8
             , GLFW.DisplayDepthBits 24
             ]

  -- open window
  if fullScreenRequest
    then openWindow ( GL.Size (fromIntegral $ videoWidth fullscreenMode) (fromIntegral $ videoHeight fullscreenMode) )
                    bits
                    GLFW.FullScreen
    else openWindow ( GL.Size 1024 768 )
                    bits
                    GLFW.Window

  GLFW.windowPos      $= Position 200 200
  glids <- initGL
  -- setup stuff
  GLFW.swapInterval       $= 1 -- vsync
  GLFW.windowTitle        $= "Interpol ate me"
  GLFW.windowSizeCallback $= resize projMat
  -- main loop
  now <- get GLFW.time
  loop waitForPress state (realToFrac now) glids
  -- exit
  cleanUpGLStuff glids
  GLFW.closeWindow
  GLFW.terminate