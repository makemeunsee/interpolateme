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
                          , viewMatOf
                          , orbitingEyeForModel
                          , orbitCenterDirection
                          , updateOrbitAngles
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
import Json


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


data KeyState = KeyState { up :: KeyButtonState
                         , down :: KeyButtonState
                         , left :: KeyButtonState
                         , right :: KeyButtonState
                         , ctrl :: KeyButtonState
                         , tab :: KeyButtonState
                         }


data MouseState = MouseState { mouseX :: GLint
                             , mouseY :: GLint
                             , wheel :: Int
                             , leftButton :: KeyButtonState
                             }
                  deriving Show


data GlobalState = GlobalState { camera :: OrbitingStatef
                               , light :: OrbitingStatef
                               , mouse :: MouseState
                               , zoom :: GLfloat     -- scale for the model
                               , modelMat :: Mat44f
                               , modelId :: Int
                               , models :: [FlatModelGL]
                               , seed :: RND.Seed
                               , glids :: GLIDs
                               , static :: Bool
                               , realTime :: GLfloat
                               , simTime :: GLfloat
                               , keys :: KeyState
                               , projMat :: IORef Mat44f
                               }


scaledModelMat :: GlobalState -> Mat44f
scaledModelMat global = G.scale (zoom global) identity `G.multMat` (modelMat global)


-- state inits


defaultKeyState :: KeyState
defaultKeyState = KeyState Release Release Release Release Release Release


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


gaussianNoise :: RealFloat a => a -> a -> a -> (a, a)
gaussianNoise variance r0 r1 = (root * cos r12Pi, root * sin r12Pi)
  where root = -2 * log r0 * variance
        r12Pi = r1 * 2 * pi


gaussianList :: RealFloat a => a -> [a] -> [a]
gaussianList variance (x0:x1:xs) = (g0 : g1 : gaussianList variance xs)
                                    where (g0, g1) = gaussianNoise variance x0 x1
gaussianList _ l = l


clamped :: RealFloat a => a -> [a] -> [a]
clamped a = if a < 0 then clamped0 (-a)
                     else clamped0 a
             where clamped0 _ [] = []
                   clamped0 absMax (x : xs) = (min (max (-absMax) x) absMax) : clamped0 absMax xs


-- data buffer functions

-- random translate faces along an axis.
axisRndFacesToFlatTriangles :: RND.Seed -> GLfloat -> Point3GL -> FlatModelGL -> ([GLfloat], RND.Seed)
axisRndFacesToFlatTriangles seed span (G.Point3f nx ny nz) m =
  (applyTranslationsToVertice rnds nx ny nz (vertice m) (verticePerFace m), newSeed)
  where -- clamped gaussian distribution
        rnds = clamped (2*span) $ gaussianList (sqrt span) rawRnds
        -- uniform distribution
        (rawRnds, newSeed) = RND.random_list (RND.range_random (0, span)) faceCount seed
        faceCount = length $ verticePerFace m


-- assign a random code between 0 and 3 to each vertex
-- vertice of the a single face have the same code
rndAnimationCodeData :: RND.Seed -> GLint -> [[Int]] -> ([GLint], RND.Seed)
rndAnimationCodeData seed _ [] = ([], seed)
rndAnimationCodeData seed k (face:faces) = (replicate l rnd ++ rem, lastSeed)
  where (rnd, newSeed) = RND.range_random (0, k) seed
        l = length faces
        (rem, lastSeed) = rndAnimationCodeData newSeed k faces


-- randomize the position of a polyhedron faces in a way imperceptible to the given (ortho) camera, relative to model transform
rndVertexBufferData :: RND.Seed -> Point3GL -> FlatModelGL -> ([GLfloat], RND.Seed)
rndVertexBufferData seed camEye model =
  axisRndFacesToFlatTriangles seed (G.norm camEye) (G.normalized camEye) model


-- how many to draw
indexCount :: [GLuint] -> GLint
indexCount polyIndice = fromIntegral $ length polyIndice


-- GL Stuff
data ShaderInfo = ShaderInfo { prog :: Program
                             , vertexAttrib :: GLuint
                             , normalAttrib :: GLuint
                             , altVertexAttrib :: GLuint
                             , centerAttrib :: GLuint
                             }


data BuffersInfo = BuffersInfo { indice :: BufferObject
                              , indiceCount :: GLint
                              , vertexBuffersLength :: Int
                              , vertexBufferId :: GLuint
                              , normalBufferId :: GLuint
                              , altVertexBufferId :: GLuint
                              , centerBufferId :: GLuint
                              }


data GLIDs = GLIDs { shaderInfo :: ShaderInfo
                   , buffersInfo :: BuffersInfo
                   }


createShader :: IO ShaderInfo
createShader = do
  prog <- loadProgram "polyhedra.vert" "polyhedra.frag"
  (AttribLocation vertexAttrib) <- get (attribLocation prog "position")
  (AttribLocation normalAttrib) <- get (attribLocation prog "normal")
  (AttribLocation altVertexAttrib) <- get (attribLocation prog "alt_position")
  (AttribLocation centerAttrib) <- get (attribLocation prog "a_barycentrics")
  return ShaderInfo{..}


loadBuffers :: [GLfloat] -> [GLfloat] -> [GLuint] -> [GLfloat] -> IO BuffersInfo
loadBuffers verticeData normalData indiceData centersData = do

  indice <- makeBuffer ElementArrayBuffer indiceData
  let indiceCount = indexCount indiceData

  -- initially, vertice and alt vertice are equal
  let vertexBuffersLength = length verticeData
  vertexBufferId <- fillNewFloatBuffer verticeData
  normalBufferId <- fillNewFloatBuffer normalData
  altVertexBufferId <- fillNewFloatBuffer verticeData
  centerBufferId <- fillNewFloatBuffer centersData
  return BuffersInfo{..}


initGL :: Bool -> Bool -> [GLfloat] -> [GLfloat] -> [GLuint] -> [GLfloat] -> IO GLIDs
initGL drawFront drawBack verticeData normalData indiceData centersData = do
  GL.depthFunc $= Just GL.Less
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.shadeModel $= GL.Smooth
--  ambient (Light 0) $= Color4 0.1 0.1 0.1 (1 :: GLfloat)
--  diffuse (Light 0) $= Color4 0.9 0.9 0.9 (1 :: GLfloat)
--  light (Light 0) $= Enabled

  let front = if drawFront then GL.Fill else GL.Line
  let back = if drawBack then GL.Fill else GL.Line
  GL.polygonMode $= (front, back)

--  GL.cullFace $= Just Back
  GL.cullFace $= Nothing

  -- make shaders & data
  shaderInfo <- Main.createShader

  -- create data buffers
  buffersInfo <- loadBuffers verticeData normalData indiceData centersData

  return GLIDs{..}


cleanBuffers :: BuffersInfo -> IO ()
cleanBuffers BuffersInfo{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  with normalBufferId $ glDeleteBuffers 1
  with altVertexBufferId $ glDeleteBuffers 1
  with centerBufferId $ glDeleteBuffers 1


-- rendering code


bindGeometry :: GLIDs -> IO ()
bindGeometry GLIDs{..} = do let ShaderInfo{..} = shaderInfo
                            let BuffersInfo{..} = buffersInfo
                            bindFloatBufferToAttrib 3 vertexBufferId vertexAttrib
                            bindFloatBufferToAttrib 3 normalBufferId normalAttrib
                            bindFloatBufferToAttrib 3 altVertexBufferId altVertexAttrib
                            bindFloatBufferToAttrib 3 centerBufferId centerAttrib


unbindGeometry :: GLIDs -> IO ()
unbindGeometry GLIDs{..} = do let ShaderInfo{..} = shaderInfo
                              glDisableVertexAttribArray vertexAttrib
                              glDisableVertexAttribArray normalAttrib
                              glDisableVertexAttribArray altVertexAttrib
                              glDisableVertexAttribArray centerAttrib


bindUniformMatrix :: Program -> String -> Mat44f -> IO ()
bindUniformMatrix prog uName mat = do
  (UniformLocation matLoc) <- get $ uniformLocation prog uName
  with mat $ glUniformMatrix4fv matLoc 1 (fromBool True) . castPtr


bindUniformVector :: Program -> String -> Vec4f -> IO ()
bindUniformVector prog uName vec = do
  (UniformLocation vLoc) <- get $ uniformLocation prog uName
  with vec $ glUniform4fv vLoc 1 . castPtr


render :: GLfloat -> Point3GL -> Mat44f -> Mat44f -> GLIDs -> IO ()
render t lightDirection vMat mvpMat glids@GLIDs{..} = do
  let ShaderInfo{..} = shaderInfo
  let BuffersInfo{..} = buffersInfo

  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

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
  bindGeometry glids

  -- bind indice
  bindBuffer ElementArrayBuffer $= Just (indice)

  drawElements GL.Triangles indiceCount GL.UnsignedInt offset0

  unbindGeometry glids

  GLFW.swapBuffers


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
  nowD <- get time
  case b of
    -- when button is released, switch back back to
    -- waitForPress action
    GLFW.Release -> return (Action (waitForPress, applyMouseMove b x y wheel))
    GLFW.Press   -> return (Action (waitForRelease, applyMouseMove b x y wheel))


triggerReshape :: GlobalState -> IO GlobalState
triggerReshape state@GlobalState{..} = do
  let BuffersInfo{..} = buffersInfo glids

  -- read current vertex buffers
  oldVertice <- readBuffer vertexBufferId vertexBuffersLength
  oldAltVertice <- readBuffer altVertexBufferId vertexBuffersLength

  -- vertex buffer becomes interpolation (as of now) of vertice and alt vertice
  refillBuffer vertexBufferId $ interpolate simTime oldVertice oldAltVertice

  -- create new alt vertice ligned with the cam
  let (newVertice, newSeed) = rndVertexBufferData seed (orbitingEyeForModel (scaledModelMat state) camera) (models !! modelId)
  refillBuffer altVertexBufferId newVertice

  -- updating geometries can be long, update realTime after
  now <- get time
  return state { realTime = realToFrac now, simTime = 0, seed = newSeed }


updateSimState :: MouseState -> GlobalState -> IO (GlobalState)
updateSimState newMouse state@GlobalState{..} = do
  t <- get time
  let now = realToFrac t
  let dt = now - realTime
  case (leftButton newMouse, leftButton mouse) of
    -- sim paused, do nothing
    (Press, _)         -> return state { mouse = newMouse, realTime = now }
    -- sim runs, but max sim time is pi, so that interpolation stops matching the alt vertice
    (Release, Release) -> return state { mouse = newMouse, realTime = now, simTime = min pi (simTime + dt) }
    -- sim restarted
    (Release, Press)   -> triggerReshape state { mouse = newMouse }


-- rotate model matrix on arrow keys released
handleKeys :: GlobalState -> IO (GlobalState)
handleKeys state = do
  let modelMatrix = modelMat state
  let ks@KeyState{..} = keys state

  -- read key inputs
  u <- GLFW.getKey GLFW.UP
  d <- GLFW.getKey GLFW.DOWN
  l <- GLFW.getKey GLFW.LEFT
  r <- GLFW.getKey GLFW.RIGHT
  ctrl <- GLFW.getKey GLFW.LCTRL
  t <- GLFW.getKey GLFW.TAB

  let (uR, dR, lR, rR, rT) = ( released u up
                             , released d down
                             , released l left
                             , released r right
                             , released t tab
                             )

  -- rotate model matrix when arrow key released
  let newMat0 = case (uR, dR) of
                 (False, False) -> modelMatrix
                 (True, _) -> G.multMat G.negXRot modelMatrix
                 (False, True) -> G.multMat G.posXRot modelMatrix

  let newMat1 = case (lR, rR) of
                 (False, False) -> newMat0
                 (True, _) -> G.multMat G.negYRot newMat0
                 (False, True) -> G.multMat G.posYRot newMat0

  let newKS = ks { up = u, down = d, left = l, right = r, ctrl = ctrl, tab = t }

  -- if tab was released, change model
  newState <- if rT then nextModel state
                    else return state

  -- if a rotation was applied, trigger reshape, else just update state
  if uR || dR || lR || rR
    then triggerReshape newState { keys = newKS, modelMat = newMat1 }
    else return newState { keys = newKS, modelMat = newMat1 }

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


updateLight :: MouseState -> GlobalState -> GlobalState
updateLight newMouseState global =
  global { light = newLight }
  where
    oldMouse = mouse global
    -- get new cam state from mouse actions
    newLight0 = updateOrbitState oldMouse newMouseState (light global)

    a = 1.05 ** fromIntegral wheelDiff
    wheelDiff = (wheel newMouseState) - (wheel $ mouse global)
    -- update intensity (= distance of orbiting state)
    newLight = newLight0 { distance = distance newLight0 / a }


loop :: Bool -> IO Action -> GlobalState -> IO GlobalState
loop static action global = do

  -- read keyboard actions & update global state
  newGlobal0 <- handleKeys global

  -- read mouse actions
  Action (action', mouseStateUpdater) <- action

  -- get new mouse state from mouse actions
  let newMouseState = mouseStateUpdater $ mouse newGlobal0

  -- update the view related properties in the global state
  let newGlobal1 = if Release == ctrl (keys newGlobal0)
                     then updateCam newMouseState newGlobal0
                     else updateLight newMouseState newGlobal0

  -- check for sim restart, update sim & gl states if necessary
  newGlobal <- if static
                 then return newGlobal1 { mouse = newMouseState }
                 else updateSimState newMouseState newGlobal1

  -- prepare matrices for rendering
  p <- get $ projMat newGlobal
  let span = FlatModel.span $ models newGlobal !! modelId newGlobal
  let scaledP = G.scale (1/span) p
  let view = viewMatOf $ camera newGlobal
  let vp = scaledP `G.multMat` view
  let mvp = vp `G.multMat` (scaledModelMat newGlobal)

  -- light direction
  let lightDirection = orbitCenterDirection $ light newGlobal

  -- render
  render (simTime newGlobal) lightDirection (view `G.multMat` modelMat newGlobal) mvp (glids newGlobal)

  -- exit if window closed or Esc pressed
  esc <- GLFW.getKey GLFW.ESC
  q <- GLFW.getKey 'Q'
  open <- GLFW.getParam GLFW.Opened
  if open && esc /= GLFW.Press && q /= GLFW.Press
    then loop static action' newGlobal
    else return newGlobal


boolArgument :: String -> [String] -> Bool
boolArgument arg args = [] /= filter ((==) arg) args


parseJsonArgs :: String -> (FilePath, [Int])
parseJsonArgs args = (head split, map read $ tail split)
  where split = splitOn "," args


nextModel :: GlobalState -> IO GlobalState
nextModel global@GlobalState{..} = do
  let GLIDs{..} = glids
  let newModelId = (1 + modelId) `mod` (length models)

  -- load next model
  let m@(FlatModel vs fs ns cs ids vpf span) = models !! newModelId

  -- update cam to properly view new model
  let newCamera = camera { distance = span * 1.1 }

  -- clean gl buffers and recreate them with proper data
  cleanBuffers buffersInfo

  let bufferMaker = if static then (\ m _ -> (vertice m, defaultSeed))
                              else let camEye = orbitingEyeForModel identity newCamera in
                                   (\ m s -> rndVertexBufferData s camEye m)
  let (vertexBufferData, newSeed) = bufferMaker m seed
  newBuffersInfo <- loadBuffers vertexBufferData ns ids cs
  let newGlids = glids { buffersInfo = newBuffersInfo }

  return global { modelId = newModelId, glids = newGlids, camera = newCamera, seed = newSeed }


main :: IO ()
main = do

--  putStrLn $ show $

  args <- getArgs

  -- fullscreen flag
  let fullScreenRequest = boolArgument "--f" args

  -- animated or not
  let static = boolArgument "--s" args

  -- invert back / front faces
  let invertFaces = boolArgument "--i" args

  -- invert back / front faces
  let bothFaces = boolArgument "--b" args

  let jsonArgs = listToMaybe $ drop 1 $ dropWhile ("--json" /=) args
  let (jsonFile, indice) = maybe (Nothing, [])
                                 ((\ (x,y) -> (Just x, y)) . parseJsonArgs)
                                 jsonArgs

  -- initialize early to have access to time
  GLFW.initialize

  -- model from json?
  t0 <- get time
  json <- readJson jsonFile
  t1 <- get time
  putStr "Json read duration: "
  putStrLn $ show $ t1 - t0

  let rawModel = maybe []
                       (\j -> [parseJson indice j])
                       json

  t2 <- get time
  putStr $ "Json parse time: "
  putStrLn $ show $ t2 - t1

  let models = map fromModel
                   (rawModel ++ polyhedrons)

  putStrLn "'Enhanced' model data summary"
  putStr "\tvertex count: "
  putStrLn $ show $ length $ vertice $ models !! 0

  t3 <- get time
  putStr $ "Model conversion time: "
  putStrLn $ show $ t3 - t2

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
  glstuff <- initGL (bothFaces || not invertFaces)
                    (bothFaces || invertFaces)
                    []
                    []
                    []
                    []

  -- init global state
  projMat <- newIORef identity
  let state0 = GlobalState { camera = defaultCamState
                           , models = models
                           , modelId = -1
                           , light = defaultLightState
                           , mouse = defaultMouseState
                           , seed = defaultSeed
                           , glids = glstuff
                           , realTime = 0
                           , simTime = 0
                           , keys = defaultKeyState
                           , projMat = projMat
                           , modelMat = identity
                           , zoom = 1
                           , static = static
                           }

  state <- nextModel state0

  -- setup stuff
  GLFW.swapInterval       $= 1 -- vsync
  GLFW.windowTitle        $= "Interpol ate me"
  GLFW.windowSizeCallback $= resize projMat
  -- main loop
  lastState <- loop static waitForPress state
  -- exit
  cleanBuffers $ buffersInfo $ glids lastState
  GLFW.closeWindow
  GLFW.terminate