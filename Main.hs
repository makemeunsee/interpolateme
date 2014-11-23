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
                                     , glBufferData
                                     , glGetBufferSubData
                                     , glGenBuffers
                                     , gl_STATIC_DRAW
                                     )
import Graphics.GLUtil

import Foreign
import Data.Maybe (listToMaybe)
import Data.ByteString.Char8 (pack)
import Data.IORef (IORef, newIORef)
import Data.Vec (Mat44, identity)
import Random.MWC.Pure (Seed)

import FloretSphere
import Geometry ( Point3f (Point3f)
                , Model (Model), vertice, faces
                , normalized, cross, times, norm
                , facesToFlatTriangles
                , axisRndFacesToFlatTriangles
                , facesToCenterFlags
                , facesToFlatIndice
                , scale
                , lookAtMatrix
                , orthoMatrix
                , multMat
                , vec3
                , defaultSeed
                )
import ListUtil
import Json


data Action = Action (IO Action, CameraState -> CameraState)

type Mat44f = Mat44 GLfloat

data CameraState = CameraState { theta :: GLdouble
                               , phi :: GLdouble
                               , distance :: GLdouble
                               , mouseX :: GLint
                               , mouseY :: GLint
                               , wheel :: Int
                               , zoom :: Float
                               , mouseDown :: Bool
                               , projMat :: IORef Mat44f
                               , viewMat :: Mat44f
                               }

data GlobalState = GlobalState { camera :: CameraState
                               , model :: Model
                               , seed :: Seed
                               , glids :: GLIDs
                               , realTime :: Double
                               , simTime :: Double
                               }

-- data buffers


-- randomize the position of a polyhedron faces in a way imperceptible to the given (ortho) camera
rndVertexBufferData :: Seed -> CameraState -> Model -> ([GLfloat], Seed)
rndVertexBufferData seed state poly = (map realToFrac floats, newSeed)
  where (floats, newSeed) = axisRndFacesToFlatTriangles seed (realToFrac $  distance state) (camLookAxis state) (vertice poly) (faces poly)


regularVertexBufferData :: Model -> [GLfloat]
regularVertexBufferData poly = map realToFrac $ facesToFlatTriangles (vertice poly) (faces poly)

-- attribute buffer to distinguish edge points from face center points
centerBufferData :: Model -> [GLfloat]
centerBufferData poly =
  map realToFrac $ facesToCenterFlags $ faces poly


-- indice linked to the vertex buffer
indexBufferData :: Model -> [GLuint]
indexBufferData poly =
  map fromIntegral $ facesToFlatIndice $ faces poly


-- how many to draw
indexCount :: [GLuint] -> GLint
indexCount polyIndice = fromIntegral $ length polyIndice


-- interpolation mimicking that of polyhedra.vert
interpolate :: Double -> [GLfloat] -> [GLfloat] -> [GLfloat]
interpolate t from to = map interp $ zip from to
  where alpha = realToFrac $ 0.5 + 0.5 * cos t
        interp (f0,f1) = alpha*f0 + (1-alpha)*f1


-- GL Stuff
data GLIDs = GLIDs { prog :: Program
                   , indice :: BufferObject
                   , indiceCount :: GLint
                   , vertexBuffersLength :: Int
                   , vertexBufferId :: GLuint
                   , vertexAttrib :: GLuint
                   , altVertexBufferId :: GLuint
                   , altVertexAttrib :: GLuint
                   , centerBufferId :: GLuint
                   , centerAttrib :: GLuint
                   }


initGL :: [GLfloat] -> [GLuint] -> [GLfloat] -> IO GLIDs
initGL verticeData indiceData centersData = do
  GL.depthFunc $= Just GL.Less
  GL.blend $= Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.shadeModel $= GL.Smooth
--  ambient (Light 0) $= Color4 0.1 0.1 0.1 (1 :: GLfloat)
--  diffuse (Light 0) $= Color4 0.9 0.9 0.9 (1 :: GLfloat)
--  light (Light 0) $= Enabled

  GL.polygonMode $= (Fill, Line)

--  GL.cullFace $= Just Back
  GL.cullFace $= Nothing

  -- make shaders & data
  prog <- loadProgram "polyhedra.vert" "polyhedra.frag"
  (AttribLocation vertexAttrib) <- get (attribLocation prog "position")
  (AttribLocation altVertexAttrib) <- get (attribLocation prog "alt_position")
  (AttribLocation centerAttrib) <- get (attribLocation prog "a_barycentric")
  indice <- makeBuffer ElementArrayBuffer indiceData
  let indiceCount = indexCount indiceData

  -- initially, vertice and alt vertice are equal
  let vertexBuffersLength = length verticeData
  vertexBufferId <- fillNewBuffer verticeData
  altVertexBufferId <- fillNewBuffer verticeData
  centerBufferId <- fillNewBuffer centersData

  return GLIDs{..}


cleanUpGLStuff :: GLIDs -> IO ()
cleanUpGLStuff GLIDs{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  with altVertexBufferId $ glDeleteBuffers 1
  with centerBufferId $ glDeleteBuffers 1


-- rendering code


bindGeometry :: GLIDs -> IO ()
bindGeometry GLIDs{..} = do bindFloatBufferToAttrib 3 vertexBufferId vertexAttrib
                            bindFloatBufferToAttrib 3 altVertexBufferId altVertexAttrib
                            bindFloatBufferToAttrib 1 centerBufferId centerAttrib


unbindGeometry :: GLIDs -> IO ()
unbindGeometry GLIDs{..} = do glDisableVertexAttribArray vertexAttrib
                              glDisableVertexAttribArray altVertexAttrib
                              glDisableVertexAttribArray centerAttrib


render :: Double -> Mat44f -> GLIDs -> IO ()
render t mvMat glids@GLIDs{..} = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  currentProgram $= Just prog

  -- bind view projection matrix
  (UniformLocation mvpLoc) <- get $ uniformLocation prog "u_worldView"
  with mvMat $ glUniformMatrix4fv mvpLoc 1 (fromBool True) . castPtr

  -- bind uniform colors
  colLoc <- get $ uniformLocation prog "u_color"
  bColLoc <- get $ uniformLocation prog "u_borderColor"
  uniform colLoc $= Color4 1 1 1 (1 :: GLfloat)
  uniform bColLoc $= Color4 0.4 0.4 0.4 (1 :: GLfloat)

  -- bind time
  timeLoc <- get $ uniformLocation prog "u_time"
  uniform timeLoc $= Index1 (realToFrac t :: GLfloat)

  -- bind attributes (position and alt position and center flags)
  bindGeometry glids
  -- bind indice
  bindBuffer ElementArrayBuffer $= Just (indice)

  drawElements Triangles indiceCount UnsignedInt offset0

  unbindGeometry glids

  GLFW.swapBuffers


-- shader / buffer creation, low level opengl code


withNewPtr f = alloca (\p -> f p >> peek p)


fillNewBuffer :: [GLfloat] -> IO GLuint
fillNewBuffer list = do
  bufId <- withNewPtr (glGenBuffers 1)
  glBindBuffer gl_ARRAY_BUFFER bufId
  withArrayLen list $ \length ptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral (length * sizeOf (undefined :: GLfloat)))
                 (ptr :: Ptr GLfloat) gl_STATIC_DRAW
  return bufId


refillBuffer :: GLuint -> [GLfloat] -> IO ()
refillBuffer bufId list = do
  glBindBuffer gl_ARRAY_BUFFER bufId
  withArrayLen list $ \length ptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral (length * sizeOf (undefined :: GLfloat)))
                 (ptr :: Ptr GLfloat) gl_STATIC_DRAW


readBuffer :: GLuint -> Int -> IO [GLfloat]
readBuffer bufId length = do
  glBindBuffer gl_ARRAY_BUFFER bufId
  arrPtr <- mallocArray length
  glGetBufferSubData gl_ARRAY_BUFFER
                     0
                     (fromIntegral (length * sizeOf (undefined :: GLfloat)))
                     (arrPtr :: Ptr GLfloat)
  list <- peekArray length arrPtr
  free arrPtr
  return list


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


defaultCamState :: CameraState
defaultCamState = CameraState { theta = pi / 2
                              , phi = pi / 2
                              , distance = 50
                              , mouseX = 0
                              , mouseY = 0
                              , wheel = 0
                              , zoom = 1
                              , mouseDown = False
                              , viewMat = identity
                              }


camStateWithMatrice :: CameraState -> IORef Mat44f -> CameraState
camStateWithMatrice state projMat = state { projMat = projMat }


camPos :: CameraState -> (GLdouble, GLdouble, GLdouble)
camPos cam = (d * sin (phi cam) * cos (theta cam), d * cos (phi cam), d * sin (phi cam) * sin (theta cam))
  where d = distance cam


camLookAxis :: CameraState -> Point3f
camLookAxis state = (-1) `times` (camToPoint3f state)


camToPoint3f :: CameraState -> Point3f
camToPoint3f cam = Point3f (realToFrac x) (realToFrac y) (realToFrac z)
  where (x,y,z) = camPos cam


-- input handling / UI


resize :: Float -> IORef Mat44f -> GLFW.WindowSizeCallback
resize span projMatRef size@(Size w h) = do
  let hh = if h < 0 then 1 else h
  let aspect = (fromIntegral w) / (fromIntegral hh)
  GL.viewport   $= (Position 0 0, size)

  let s = realToFrac (span * 1.5) :: GLfloat
  let far = 3*s
  let near = -s
  let right = s * aspect
  let top = s
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


updateZoom :: Int -> CameraState -> CameraState
updateZoom newWheel state = state { wheel = newWheel
                                  , zoom = newZoom
                                  }
  where newZoom = max (min (zoom0*a) 8) 0.125
        zoom0 = zoom state
        oldWheel = wheel state
        a = 1.1 ** fromIntegral (oldWheel-newWheel)


onClick :: GLint -> GLint -> Int -> CameraState -> CameraState
onClick newX newY newWheel state =
  updateZoom newWheel state { mouseX = newX
                            , mouseY = newY
                            , mouseDown = True
                            }


applyMouseMove :: Bool -> GLint -> GLint -> Int -> CameraState -> CameraState
applyMouseMove mouseDown newX newY newWheel state =
  updateZoom newWheel state  { mouseX = newX
                             , mouseY = newY
                             , mouseDown = mouseDown
                             , theta = (theta state) + (fromIntegral diffX) / 100.0
                             , phi = limitAngle $ (phi state) + (fromIntegral diffY) / 100.0
                             }
  where
    diffX = newX - mouseX state
    diffY = -newY + mouseY state


waitForPress :: IO Action
waitForPress = do
  b <- GLFW.getMouseButton GLFW.ButtonLeft
  wheel <- get mouseWheel
  case b of
    GLFW.Release -> return (Action (waitForPress, updateZoom wheel))
    GLFW.Press   -> do
      -- when left mouse button is pressed,
      -- switch to waitForRelease action.
      (GL.Position x y) <- GL.get GLFW.mousePos
      return (Action (waitForRelease, onClick x y wheel))


waitForRelease :: IO Action
waitForRelease = do
  -- keep track of mouse movement while waiting for button
  -- release
  (GL.Position x y) <- GL.get GLFW.mousePos
  b <- GLFW.getMouseButton GLFW.ButtonLeft
  wheel <- get mouseWheel
  nowD <- get time
  case b of
    -- when button is released, switch back back to
    -- waitForPress action
    GLFW.Release -> return (Action (waitForPress, applyMouseMove False x y wheel))
    GLFW.Press   -> return (Action (waitForRelease, applyMouseMove True x y wheel))


updateState :: GlobalState -> CameraState -> IO (GlobalState)
updateState state@GlobalState{..} newCamera = do
  now <- get time
  let dt = now - realTime
  let GLIDs{..} = glids
  case (mouseDown newCamera, mouseDown camera) of
    -- sim paused, do nothing
    (True, _)      -> return state { camera = newCamera, realTime = now }
    -- sim runs, but max sim time is pi, so that interpolation stops matching the alt vertice
    (False, False) -> return state { camera = newCamera, realTime = now, simTime = min pi (simTime + dt) }
    -- sim restarted
    (False, True)  -> do
      oldVertice <- Main.readBuffer vertexBufferId vertexBuffersLength
      oldAltVertice <- Main.readBuffer altVertexBufferId vertexBuffersLength
      refillBuffer vertexBufferId $ interpolate simTime oldVertice oldAltVertice
      let (newVertice, newSeed) = rndVertexBufferData seed newCamera model
      refillBuffer altVertexBufferId newVertice
      return state { camera = newCamera, realTime = now, simTime = 0, seed = newSeed }


loop :: Bool -> IO Action -> GlobalState -> IO ()
loop static action state = do

  -- read io actions
  Action (action', camUpdater) <- action

  -- update state with mouse actions
  let camState0 = camUpdater $ camera state

  -- apply zoom
  let k = (zoom $ camera state) / (zoom camState0)
  if (k /= 1)
    then do
      let projRef = projMat $ camera state
      oldProjection <- get projRef
      projRef $= Geometry.scale (realToFrac k) oldProjection
    else return ()

  -- update camera
  let (Point3f px py pz) = camToPoint3f camState0
  let camState1 = camState0 { viewMat = lookAtMatrix (vec3 (realToFrac px) (realToFrac py) (realToFrac pz))
                                                     (vec3 0 0 0)
                                                     (vec3 0 1 0)
                            }

  -- check for sim restart
  newState <- if static
                then return state { camera = camState1 }
                else updateState state camState1

  -- render
  projection <- get $ projMat camState1

  render (simTime newState) (multMat projection $ viewMat camState1) (glids newState)

  -- exit if window closed or Esc pressed
  esc <- GLFW.getKey GLFW.ESC
  q <- GLFW.getKey 'Q'
  open <- GLFW.getParam GLFW.Opened
  if open && esc /= GLFW.Press && q /= GLFW.Press
    then loop static action' newState
    else return ()


main :: IO ()
main = do

--  putStrLn $ show $

  args <- getArgs

  -- fullscreen flag
  let fullScreenRequest = [] /= filter (\s -> s == "--f") args

  -- animated or not
  let static = [] /= filter (\s -> s == "--s") args

  -- model from json?
  json <- readJson $ listToMaybe $ drop 1 $ dropWhile ("--json" /=) args

  let model = maybe pentagonalHexecontahedron
                    parseJson
                    json
  let span = maximum $ map norm $ vertice model


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

  -- init GL state
  projMat <- newIORef identity
  let camState = (camStateWithMatrice defaultCamState projMat) { distance = realToFrac span * 1.1 }
  let bufferMaker = if static then (\ m s -> (regularVertexBufferData m, defaultSeed))
                              else (\ m s -> rndVertexBufferData s camState m)
  let (vertexBufferData, seed0) = bufferMaker model defaultSeed
  let centersBuffer = centerBufferData model
  let indiceBuffer = indexBufferData model

  glids <- initGL vertexBufferData indiceBuffer centersBuffer

  -- init global state
  now <- get time
  let state = GlobalState { camera = camState
                          , model = model
                          , seed = seed0
                          , glids = glids
                          , realTime = 0
                          , simTime = 0
                          }


  -- setup stuff
  GLFW.swapInterval       $= 1 -- vsync
  GLFW.windowTitle        $= "Interpol ate me"
  GLFW.windowSizeCallback $= resize span projMat
  -- main loop
  now <- get GLFW.time
  loop static waitForPress state
  -- exit
  cleanUpGLStuff glids
  GLFW.closeWindow
  GLFW.terminate