{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (
    main
) where

import System.Environment (getArgs)

import Graphics.Rendering.OpenGL.GL ( GLfloat
                                    , GLint
                                    , GLuint
                                    , Program
                                    , BufferObject
                                    , AttribLocation (AttribLocation), attribLocation
                                    , UniformLocation (UniformLocation), uniformLocation
                                    , get, uniform
                                    , BufferTarget (ElementArrayBuffer)
                                    , currentProgram, bindBuffer, createProgram, linkProgram
                                    , validateProgram, attachShader, detachShader, releaseShaderCompiler
                                    , linkStatus, validateStatus, programInfoLog
                                    , drawElements
                                    )
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL.Raw ( glUniformMatrix4fv
                                     , glUniform4fv
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

import Control.Exception
import Foreign
import Foreign.C.Types (CFloat, CInt)
import Data.Maybe (listToMaybe)
import Data.IORef (IORef, newIORef)
import Data.Vec (Mat44, Vec4, multmv, identity)
import Data.List.Split (splitOn)
import qualified Random.MWC.Pure as RND

import FloretSphere
import qualified Geometry as G
import GLGenericFunctions ( OrbitingState (OrbitingState), theta, phi, distance, thetaSpeed, phiSpeed
                          , interpolate
                          , viewMatOf, latLongRotMat
                          , orbitingEyeForModel
                          , orbitCenterDirection
                          , orbitingPosition
                          , updateOrbitAngles
                          , latLongPosition
                          )
import FlatModel ( facesToFlatTriangles
                 , facesToCenterFlags
                 , facesToFlatIndice
                 , normalsToFlatNormals
                 , applyTranslationsToVertice
                 , FlatModel (FlatModel), vertice, normals, verticePerFace, span
                 , fromModel
                 )
import ListUtil
import Voronyi


-- NearZero instance for GLfloat
import Data.Vec.LinAlg (NearZero(..))
instance NearZero CFloat where
  nearZero x = abs x < 1e-6
  {-# INLINE nearZero #-}


type OrbitingStatef = OrbitingState GLfloat
type Vec4f = Vec4 GLfloat
type Mat44f = Mat44 GLfloat
type Point3GL = G.Point3f GLfloat
type FlatModelGL = FlatModel GLfloat GLuint


data Action = Action (IO Action, MouseState -> MouseState)


data KeyState = KeyState { tab :: KeyButtonState
                         , space :: KeyButtonState
                         , ctrl :: KeyButtonState
                         }


data MouseState = MouseState { mouseX :: GLint
                             , mouseY :: GLint
                             , wheel :: Int
                             , leftButton :: KeyButtonState
                             , rightButton :: KeyButtonState
                             }
                  deriving Show


data GlobalState = GlobalState { camera :: OrbitingStatef
                               , light :: OrbitingStatef
                               , cutPlane :: OrbitingStatef
                               , showCutPlane :: Bool
                               , mouse :: MouseState
                               , zoom :: GLfloat     -- scale for the model
                               , modelMat :: Mat44f
                               , model :: VoronoiModel GLfloat
                               , span :: GLfloat
                               , glids :: GLIDs
                               , simTime :: GLfloat
                               , keys :: KeyState
                               , projMat :: IORef Mat44f
                               }


scaledModelMat :: GlobalState -> Mat44f
scaledModelMat global = G.scale (zoom global) identity `G.multMat` (modelMat global)


-- state inits


defaultKeyState :: KeyState
defaultKeyState = KeyState Release Release Release


defaultMouseState :: MouseState
defaultMouseState = MouseState { mouseX = 0
                               , mouseY = 0
                               , wheel = 0
                               , leftButton = Release
                               }


defaultLightState :: OrbitingStatef
defaultLightState = OrbitingState { theta = -1.2
                                  , phi = 1.8
                                  , distance = 1
                                  , thetaSpeed = -0.005
                                  , phiSpeed = 0.005
                                  }


defaultCamState :: OrbitingStatef
defaultCamState = OrbitingState { theta = pi/2
                                , phi = pi/2
                                , distance = 50
                                , thetaSpeed = 0.005
                                , phiSpeed = 0.005
                                }


defaultCutPlaneState :: OrbitingStatef
defaultCutPlaneState = OrbitingState { theta = 0
                                     , phi = pi/2
                                     , distance = 1
                                     , thetaSpeed = 0.005
                                     , phiSpeed = 0.005
                                     }

-- rnd / num distribution functions


defaultSeed :: RND.Seed
defaultSeed = RND.seed $ map charToWord32 "defaultSeed"
  where charToWord32 c = fromIntegral $ fromEnum c


-- GLfloat RangeRandom instance
instance RND.RangeRandom CFloat where
  range_random (x0, x1) s = (realToFrac r, s')
    where (r, s') = RND.range_random(realToFrac x0 :: Float, realToFrac x1 :: Float) s


-- GLint RangeRandom instance
instance RND.RangeRandom CInt where
  range_random (x0, x1) s = (fromIntegral r, s')
    where (r, s') = RND.range_random(fromIntegral x0 :: Int, fromIntegral x1 :: Int) s


-- how many to draw
indexCount :: [GLuint] -> GLint
indexCount polyIndice = fromIntegral $ length polyIndice


-- GL Stuff
data ShaderInfo = ShaderInfo { prog :: Program
                             , vertexAttrib :: GLuint
                             , normalAttrib :: GLuint
                             , centerAttrib :: GLuint
                             }


data BuffersInfo = BuffersInfo { indice :: BufferObject
                               , indiceCount :: GLint
                               , vertexBuffersLength :: Int
                               , vertexBufferId :: GLuint
                               , normalBufferId :: GLuint
                               , centerBufferId :: GLuint
                               }


data GLIDs = GLIDs { objectShaderInfo :: ShaderInfo
                   , planeShaderInfo :: ShaderInfo
                   , objectBuffersInfo :: BuffersInfo
                   , planeBuffersInfo :: BuffersInfo
                   }


createShader :: FilePath -> FilePath -> IO ShaderInfo
createShader vertexShaderFile fragShaderFile = do
  prog <- loadProgram vertexShaderFile fragShaderFile
  (AttribLocation vertexAttrib) <- get (attribLocation prog "position")
  (AttribLocation normalAttrib) <- get (attribLocation prog "normal")
  (AttribLocation centerAttrib) <- get (attribLocation prog "a_barycentrics")
  return ShaderInfo{..}


loadBuffers :: [GLfloat] -> [GLfloat] -> [GLuint] -> [GLfloat] -> IO BuffersInfo
loadBuffers verticeData normalData indiceData centersData = do

  indice <- makeBuffer ElementArrayBuffer indiceData
  let indiceCount = indexCount indiceData

  let vertexBuffersLength = length verticeData
  vertexBufferId <- fillNewFloatBuffer verticeData
  normalBufferId <- fillNewFloatBuffer normalData
  centerBufferId <- fillNewFloatBuffer centersData

  return BuffersInfo{..}


initGL :: Bool -> Bool -> [GLfloat] -> [GLfloat] -> [GLuint] -> [GLfloat] -> IO GLIDs
initGL drawFront drawBack verticeData normalData indiceData centersData = do
  GL.depthFunc $= Just GL.Less
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.shadeModel $= GL.Smooth

  let front = if drawFront then GL.Fill else GL.Line
  let back = if drawBack then GL.Fill else GL.Line
  GL.polygonMode $= (front, back)

--  GL.cullFace $= Just Back
  GL.cullFace $= Nothing

  -- make shaders & data
  objectShaderInfo <- Main.createShader "polyhedra.vert" "polyhedra.frag"
  planeShaderInfo <- Main.createShader "plane.vert" "plane.frag"

  -- create data buffers
  objectBuffersInfo <- loadBuffers verticeData normalData indiceData centersData
  planeBuffersInfo <- loadBuffers [ 1, 0, 0
                                  , 1, 2, -2
                                  , 1, 2, 2
                                  , 1, -2, 2
                                  , 1, -2, -2
                                  ]
                                  (take (3*4) $ cycle [ 1, 0, 0 ])
                                  [ 0, 1, 2
                                  , 0, 2, 3
                                  , 0, 3, 4
                                  , 0, 4, 1
                                  ]
                                  [ 1, 1, 1
                                  , 1, 0, 0
                                  , 0, 1, 0
                                  , 1, 0, 0
                                  , 0, 1, 0]

  return GLIDs{..}


cleanBuffers :: BuffersInfo -> IO ()
cleanBuffers BuffersInfo{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  with normalBufferId $ glDeleteBuffers 1
  with centerBufferId $ glDeleteBuffers 1


-- rendering code


bindGeometry :: ShaderInfo -> BuffersInfo -> IO ()
bindGeometry ShaderInfo{..} BuffersInfo{..} = do
  bindFloatBufferToAttrib 3 vertexBufferId vertexAttrib
  bindFloatBufferToAttrib 3 normalBufferId normalAttrib
  bindFloatBufferToAttrib 3 centerBufferId centerAttrib


unbindGeometry :: ShaderInfo -> IO ()
unbindGeometry ShaderInfo{..} = do
  glDisableVertexAttribArray vertexAttrib
  glDisableVertexAttribArray normalAttrib
  glDisableVertexAttribArray centerAttrib


bindUniformMatrix :: Program -> String -> Mat44f -> IO ()
bindUniformMatrix prog uName mat = do
  (UniformLocation matLoc) <- get $ uniformLocation prog uName
  with mat $ glUniformMatrix4fv matLoc 1 (fromBool True) . castPtr


bindUniformVector :: Program -> String -> Vec4f -> IO ()
bindUniformVector prog uName vec = do
  (UniformLocation vLoc) <- get $ uniformLocation prog uName
  with vec $ glUniform4fv vLoc 1 . castPtr


prepareRender :: IO ()
prepareRender = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]


renderObject :: GLfloat -> Point3GL -> Mat44f -> Mat44f -> GLIDs -> IO ()
renderObject t lightDirection vMat mvpMat glids@GLIDs{..} = do
  let ShaderInfo{..} = objectShaderInfo
  let BuffersInfo{..} = objectBuffersInfo

  currentProgram $= Just prog

  -- bind matrices
  bindUniformMatrix prog "u_mvpMat" mvpMat
  bindUniformMatrix prog "u_vMat" vMat

  -- bind light
  -- direction is normed in shader
  let G.Point3f x y z = G.normalized lightDirection
  bindUniformVector prog "u_lightDirection" $ G.vec4 x y z
  -- intensity decreases with square distance
  let n = G.norm lightDirection
  lightILoc <- get $ uniformLocation prog "u_lightIntensity"
  uniform lightILoc $= GL.Index1 (1 / n / n)

  -- bind uniform colors
  colLoc <- get $ uniformLocation prog "u_color"
  bColLoc <- get $ uniformLocation prog "u_borderColor"
  uniform colLoc $= GL.Color4 1 1 1 (1 :: GLfloat)
  uniform bColLoc $= GL.Color4 0.4 0.4 0.4 (1 :: GLfloat)

  -- bind time
  timeLoc <- get $ uniformLocation prog "u_time"
  uniform timeLoc $= GL.Index1 t

  -- bind attributes
  bindGeometry objectShaderInfo objectBuffersInfo

  -- bind indice
  bindBuffer ElementArrayBuffer $= Just (indice)

  drawElements GL.Triangles indiceCount GL.UnsignedInt offset0

  unbindGeometry objectShaderInfo


renderPlane :: Mat44f -> GLIDs -> IO ()
renderPlane mvpMat glids@GLIDs{..} = do
  let ShaderInfo{..} = planeShaderInfo
  let BuffersInfo{..} = planeBuffersInfo

  currentProgram $= Just prog

  -- bind matrices
  bindUniformMatrix prog "u_mvpMat" mvpMat

  -- bind uniform colors
  colLoc <- get $ uniformLocation prog "u_color"
  bColLoc <- get $ uniformLocation prog "u_borderColor"
  uniform colLoc $= GL.Color4 0.8 0.3 0.3 (0.5 :: GLfloat)
  uniform bColLoc $= GL.Color4 1 0 0 (1 :: GLfloat)

  -- bind attributes
  bindGeometry planeShaderInfo planeBuffersInfo
  -- bind indice
  bindBuffer ElementArrayBuffer $= Just (indice)

  drawElements GL.Triangles indiceCount GL.UnsignedInt offset0

  unbindGeometry planeShaderInfo


-- shader / buffer creation, low level opengl code


withNewPtr f = alloca (\p -> f p >> peek p)


fillNewIntBuffer :: [GLint] -> IO GLuint
fillNewIntBuffer list = do
  bufId <- withNewPtr (glGenBuffers 1)
  glBindBuffer gl_ARRAY_BUFFER bufId
  withArrayLen list $ \length ptr ->
    glBufferData gl_ARRAY_BUFFER (fromIntegral (length * sizeOf (undefined :: GLint)))
                 (ptr :: Ptr GLint) gl_STATIC_DRAW
  return bufId


fillNewFloatBuffer :: [GLfloat] -> IO GLuint
fillNewFloatBuffer list = do
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
    [ (GL.VertexShader, vertShader)
    , (GL.FragmentShader, fragShader)
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


-- input handling / UI

resize :: IORef Mat44f -> GLFW.WindowSizeCallback
resize projMatRef size@(GL.Size w h) = do
  GL.viewport   $= (GL.Position 0 0, size)
  projMatRef $= G.orthoMatrixFromScreen w h
  return ()


updateOrbitState :: MouseState -> MouseState -> OrbitingStatef -> OrbitingStatef
updateOrbitState oldMouse newMouse orbit =
  if leftButton newMouse == Press
    then updateOrbitAngles ((fromIntegral diffX) * thetaSpeed orbit) ((fromIntegral diffY) * phiSpeed orbit) orbit
    else orbit
  where diffX = (mouseX newMouse) - (mouseX oldMouse)
        diffY = (mouseY oldMouse) - (mouseY newMouse)


updateZoom :: MouseState -> MouseState -> GlobalState -> GlobalState
updateZoom oldMouse newMouse global = global { zoom = newZoom }
  where newZoom = max (min (zoom0*a) 8) 0.125
        zoom0 = zoom global
        a = 1.1 ** fromIntegral wheelDiff
        wheelDiff = (wheel newMouse) - (wheel oldMouse)


onClick :: GLint -> GLint -> Int -> MouseState -> MouseState
onClick newX newY newWheel state =
  updateCoordsAndWheel newX newY newWheel state { leftButton = Press }


applyMouseMove :: KeyButtonState -> GLint -> GLint -> Int -> MouseState -> MouseState
applyMouseMove leftButtonDown newX newY newWheel state =
  updateCoordsAndWheel newX newY newWheel state  { leftButton = leftButtonDown }


updateCoordsAndWheel :: GLint -> GLint -> Int -> MouseState -> MouseState
updateCoordsAndWheel newX newY newWheel state = state { mouseX = newX
                                                      , mouseY = newY
                                                      , wheel = newWheel
                                                      }


waitForPress :: IO Action
waitForPress = do
  (GL.Position x y) <- GL.get GLFW.mousePos
  wheel <- get mouseWheel
  b <- GLFW.getMouseButton GLFW.ButtonLeft
  case b of
    GLFW.Release -> return (Action (waitForPress, updateCoordsAndWheel x y wheel))
    GLFW.Press   -> do
      -- when left mouse button is pressed,
      -- switch to waitForRelease action.
      return $ Action (waitForRelease, onClick x y wheel)


waitForRelease :: IO Action
waitForRelease = do
  -- keep track of mouse movement while waiting for button
  -- release
  (GL.Position x y) <- GL.get GLFW.mousePos
  b <- GLFW.getMouseButton GLFW.ButtonLeft
  wheel <- get mouseWheel
  case b of
    -- when button is released, switch back back to
    -- waitForPress action
    GLFW.Release -> return (Action (waitForPress, applyMouseMove b x y wheel))
    GLFW.Press   -> return (Action (waitForRelease, applyMouseMove b x y wheel))


-- rotate model matrix on arrow keys released
handleKeys :: GlobalState -> IO (GlobalState)
handleKeys state = do
  let modelMatrix = modelMat state
  let ks@KeyState{..} = keys state

  -- read key inputs
  tb <- GLFW.getKey GLFW.TAB
  sp <- GLFW.getKey ' '
  lctrl <- GLFW.getKey GLFW.LCTRL
  rctrl <- GLFW.getKey GLFW.RCTRL

  let (spR, tbR) = ( released sp space
                   , released tb tab
                   )

  newState0 <- if spR
    then do
      let seed = G.normalized $ latLongPosition $ cutPlane state
      let cut = Voronyi.truncate 0.00001 seed $ model state
      loadModel state cut
    else
      return state

  let newState1 = if tbR then newState0 { showCutPlane = not $ showCutPlane state }
                         else newState0

  let newKS = ks { tab = tb, space = sp, ctrl = if lctrl == Release && lctrl == rctrl then Release else Press }

  return newState1 { keys = newKS }

  where released newK oldK = newK == Release && newK /= oldK


updateCam :: MouseState -> GlobalState -> GlobalState
updateCam newMouseState global =
  newGlobal { camera = newCamera }
  where
    oldMouse = mouse global
    -- get new cam state from mouse actions
    newCamera = updateOrbitState oldMouse newMouseState (camera global)

    -- apply zoom
    newGlobal = updateZoom oldMouse newMouseState global


updateCutPlane :: MouseState -> GlobalState -> GlobalState
updateCutPlane newMouseState global =
  global { cutPlane = newCutPlane }
  where
    oldMouse = mouse global
    -- get new cam state from mouse actions
    newCutPlane = updateOrbitState oldMouse newMouseState (cutPlane global)


loop :: IO Action -> GlobalState -> IO GlobalState
loop action global = do

  -- read keyboard actions & update global state
  newGlobal0 <- handleKeys global

  -- read mouse actions
  Action (action', mouseStateUpdater) <- action

  -- get new mouse state from mouse actions
  let newMouseState = mouseStateUpdater $ mouse newGlobal0

  -- update the view related properties in the global state
  let newGlobal1 = if Release == ctrl (keys newGlobal0)
                     then updateCam newMouseState newGlobal0
                     else updateCutPlane newMouseState newGlobal0

  let newGlobal = newGlobal1 { mouse = newMouseState }

  -- prepare matrices for rendering
  p <- get $ projMat newGlobal
  let scaling = Main.span newGlobal
  let scaledP = G.scale (1/scaling) p
  let view = viewMatOf $ camera newGlobal
  let vp = scaledP `G.multMat` view
  let mvp = vp `G.multMat` (scaledModelMat newGlobal)

  let planeMvp = mvp `G.multMat` (latLongRotMat $ cutPlane newGlobal)

  -- light direction
  let lightDirection = orbitCenterDirection $ light newGlobal

  -- render
  prepareRender
  renderObject (simTime newGlobal) lightDirection (view `G.multMat` modelMat newGlobal) mvp (glids newGlobal)
  if showCutPlane newGlobal0
    then renderPlane planeMvp (glids newGlobal)
    else return ()
  GLFW.swapBuffers

  -- exit if window closed or Esc pressed
  esc <- GLFW.getKey GLFW.ESC
  q <- GLFW.getKey 'Q'
  open <- GLFW.getParam GLFW.Opened
  if open && esc /= GLFW.Press && q /= GLFW.Press
    then loop action' newGlobal
    else return newGlobal


boolArgument :: String -> [String] -> Bool
boolArgument arg args = [] /= filter ((==) arg) args


parseJsonArgs :: String -> (FilePath, [Int])
parseJsonArgs args = (head split, map read $ tail split)
  where split = splitOn "," args


loadModel :: GlobalState -> VoronoiModel GLfloat -> IO GlobalState
loadModel global@GlobalState{..} vm@VoronoiModel{..} = do
  let GLIDs{..} = glids

  -- load next model
  let FlatModel vs fs ns cs ids vpf span = fromModel $ G.Model vertice polygons seeds

  -- update cam to properly view new model
  let newCamera = camera { distance = span * 1.1 }

  -- clean gl buffers and recreate them with proper data
  cleanBuffers objectBuffersInfo

  let vertexBufferData = vs
  newBuffersInfo <- loadBuffers vs ns ids cs
  let newGlids = glids { objectBuffersInfo = newBuffersInfo }

  return global { glids = newGlids, camera = newCamera, model = vm, Main.span = span }


main :: IO ()
main = do

--  putStrLn $ show $

  args <- getArgs

  -- fullscreen flag
  let fullScreenRequest = boolArgument "--f" args

  -- draw back and front faces
  let bothFaces = boolArgument "--b" args

  -- initialize early to have access to time
  GLFW.initialize

  let model@(VoronoiModel _ vs0 fs0) = toVoronoiModel $ polyhedrons !! 0

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

  GLFW.windowPos $= GL.Position 200 200

  -- init GL state
  glstuff <- initGL (True)
                    (bothFaces)
                    []
                    []
                    []
                    []

  -- init global state
  projMat <- newIORef identity
  let state0 = GlobalState { camera = defaultCamState
                           , light = defaultLightState
                           , cutPlane = defaultCutPlaneState
                           , showCutPlane = True
                           , mouse = defaultMouseState
                           , glids = glstuff
                           , simTime = 0
                           , keys = defaultKeyState
                           , projMat = projMat
                           , modelMat = identity
                           , zoom = 1
                           }

  state <- loadModel state0 model

  -- setup stuff
  GLFW.swapInterval       $= 1 -- vsync
  GLFW.windowTitle        $= "Voronyi maze"
  GLFW.windowSizeCallback $= resize projMat
  -- main loop
  lastState <- loop waitForPress state
  -- exit
  cleanBuffers $ objectBuffersInfo $ glids lastState
  cleanBuffers $ planeBuffersInfo $ glids lastState
  GLFW.closeWindow
  GLFW.terminate