{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Foreign.C.Types (CFloat)
import Data.Maybe (listToMaybe)
import Data.ByteString.Char8 (pack)
import Data.IORef (IORef, newIORef)
import Data.Vec (Mat44, identity)
import Data.List.Split (splitOn)
import qualified Random.MWC.Pure as RND

import FloretSphere
import Geometry ( Point3f (Point3f)
                , Model (Model)
                , normalized, cross, times, norm
                , facesToFlatTriangles
                , facesToCenterFlags
                , facesToFlatIndice
                , scale
                , lookAtMatrix
                , orthoMatrix
                , multMat, multInvMatV
                , negXRot, posXRot, negYRot, posYRot
                , vec3, vec4, vec4ToPoint3f
                )
import ListUtil
import Json


-- NearZero instance for GLfloat
import Data.Vec.LinAlg (NearZero(..))
instance NearZero CFloat where
  nearZero x = abs x < 1e-6
  {-# INLINE nearZero #-}


type Mat44f = Mat44 GLfloat
type Point3GL = Point3f GLfloat


data Action = Action (IO Action, CameraState -> CameraState)


data FlatModel = FlatModel { vertice :: [GLfloat], faces :: [Int] }


data KeyState = KeyState { up :: KeyButtonState
                         , down :: KeyButtonState
                         , left :: KeyButtonState
                         , right :: KeyButtonState
                         }


data CameraState = CameraState { theta :: GLfloat
                               , phi :: GLfloat
                               , distance :: GLfloat
                               , mouseX :: GLint
                               , mouseY :: GLint
                               , wheel :: Int
                               , zoom :: GLfloat
                               , leftButton :: KeyButtonState
                               , projMat :: IORef Mat44f
                               , viewMat :: Mat44f
                               }


data GlobalState = GlobalState { camera :: CameraState
                               , model :: FlatModel
                               , seed :: RND.Seed
                               , glids :: GLIDs
                               , realTime :: GLfloat
                               , simTime :: GLfloat
                               , keys :: KeyState
                               , modelMat :: Mat44f
                               , vertexCountPerFace :: [Int]
                               }


-- rnd / num distribution functions


defaultSeed :: RND.Seed
defaultSeed = RND.seed $ map charToWord32 "defaultSeed"
  where charToWord32 c = fromIntegral $ fromEnum c


-- GLfloat RangeRandom instance
instance RND.RangeRandom CFloat where
  range_random (x0, x1) s = (realToFrac r, s')
    where (r, s') = RND.range_random(realToFrac x0 :: Float, realToFrac x1 :: Float) s


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
axisRndFacesToFlatTriangles :: RND.Seed -> GLfloat -> Point3GL -> [GLfloat] -> [Int] -> ([GLfloat], RND.Seed)
axisRndFacesToFlatTriangles seed span (Point3f nx ny nz) vertice vertexCountPerFace =
  (recurse vertice $ zip rnds vertexCountPerFace, newSeed)
  where -- clamped gaussian distribution
        rnds = clamped (2*span) $ gaussianList (sqrt $ span) rawRnds
        -- uniform distribution
        (rawRnds, newSeed) = RND.random_list (\ s -> RND.range_random (0, span) s) faceCount seed
        faceCount = length vertexCountPerFace
        recurse [] [] = []
        recurse vs ((r, count) : rfs) = translated ++ recurse rValues rfs
          where floatCount = 3*count
                values = take floatCount vs
                rValues = drop floatCount vs
                -- closer faces move closer, farther faces move farther -> minimize overlapping faces
                alpha = r * (signum r) * (signum $ nx * values !! 0 + ny * values !! 1 + nz * values !! 2)
                translation = take floatCount $ cycle [alpha*nx, alpha*ny, alpha*nz]
                translated = map (\(v,t) -> v+t) $ zip values translation


-- randomize the position of a polyhedron faces in a way imperceptible to the given (ortho) camera, relative to model transform
rndVertexBufferData :: RND.Seed -> Point3GL -> FlatModel -> [Int] -> ([GLfloat], RND.Seed)
rndVertexBufferData seed camEye model vertexCountPerFace =
  axisRndFacesToFlatTriangles seed (norm camEye) (normalized camEye) (vertice model) vertexCountPerFace


-- attribute buffer to distinguish edge points from face center points
centerBufferData :: [[Int]] -> [GLfloat]
centerBufferData faces = facesToCenterFlags faces


-- indice linked to the vertex buffer
indexBufferData :: [[Int]] -> [GLuint]
indexBufferData faces = map fromIntegral $ facesToFlatIndice faces


-- how many to draw
indexCount :: [GLuint] -> GLint
indexCount polyIndice = fromIntegral $ length polyIndice


-- interpolation mimicking that of polyhedra.vert
interpolate :: GLfloat -> [GLfloat] -> [GLfloat] -> [GLfloat]
interpolate t from to = map interp $ zip from to
  where alpha = 0.5 + 0.5 * cos t
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


initGL :: Bool -> Bool -> [GLfloat] -> [GLuint] -> [GLfloat] -> IO GLIDs
initGL drawFront drawBack verticeData indiceData centersData = do
  GL.depthFunc $= Just GL.Less
  GL.blend $= Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.shadeModel $= GL.Smooth
--  ambient (Light 0) $= Color4 0.1 0.1 0.1 (1 :: GLfloat)
--  diffuse (Light 0) $= Color4 0.9 0.9 0.9 (1 :: GLfloat)
--  light (Light 0) $= Enabled

  let front = if drawFront then Fill else Line
  let back = if drawBack then Fill else Line
  GL.polygonMode $= (front, back)

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


render :: GLfloat -> Mat44f -> GLIDs -> IO ()
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
  uniform timeLoc $= Index1 t

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


defaultKeyState :: KeyState
defaultKeyState = KeyState Release Release Release Release


defaultCamState :: CameraState
defaultCamState = CameraState { theta = pi / 2
                              , phi = pi / 2
                              , distance = 50
                              , mouseX = 0
                              , mouseY = 0
                              , wheel = 0
                              , zoom = 1
                              , leftButton = Release
                              , viewMat = identity
                              }


camStateWithMatrice :: CameraState -> IORef Mat44f -> CameraState
camStateWithMatrice state projMat = state { projMat = projMat }


camPos :: CameraState -> (GLfloat, GLfloat, GLfloat)
camPos cam = (d * sin (phi cam) * cos (theta cam), d * cos (phi cam), d * sin (phi cam) * sin (theta cam))
  where d = distance cam


camLookAxis :: CameraState -> Point3GL
camLookAxis state = (-1) `times` (camToPoint3f state)


camToPoint3f :: CameraState -> Point3GL
camToPoint3f cam = Point3f x y z
  where (x,y,z) = camPos cam


camEyeForModel :: Mat44f -> CameraState -> Point3GL
camEyeForModel modelMatrix state = vec4ToPoint3f modelCamEye
  where modelCamEye = multInvMatV modelMatrix $ vec4 ex ey ez
        camEye@(Point3f ex ey ez) = camLookAxis state


-- input handling / UI


resize :: GLfloat -> IORef Mat44f -> GLFW.WindowSizeCallback
resize span projMatRef size@(Size w h) = do
  let hh = if h < 0 then 1 else h
  let aspect = (fromIntegral w) / (fromIntegral hh)
  GL.viewport   $= (Position 0 0, size)

  let s = span * 1.5
  let far = 5*s
  let near = -3*s
  let right = s * aspect
  let top = s
  let left = -right
  let bottom = -top

  projMatRef $= orthoMatrix left
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
    else if angle > pi - 0.01
      then pi - 0.01
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
                            , leftButton = Press
                            }


mouseSpeed :: Floating a => a
mouseSpeed = 0.005


applyMouseMove :: KeyButtonState -> GLint -> GLint -> Int -> CameraState -> CameraState
applyMouseMove leftButtonDown newX newY newWheel state =
  updateZoom newWheel state  { mouseX = newX
                             , mouseY = newY
                             , leftButton = leftButtonDown
                             , theta = (theta state) + (fromIntegral diffX) * mouseSpeed
                             , phi = limitAngle $ (phi state) + (fromIntegral diffY) * mouseSpeed
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
  let GLIDs{..} = glids

  -- read current vertex buffers
  oldVertice <- Main.readBuffer vertexBufferId vertexBuffersLength
  oldAltVertice <- Main.readBuffer altVertexBufferId vertexBuffersLength

  -- vertex buffer becomes interpolation (as of now) of vertice and alt vertice
  refillBuffer vertexBufferId $ interpolate simTime oldVertice oldAltVertice

  -- create new alt vertice ligned with the cam
  let (newVertice, newSeed) = rndVertexBufferData seed (camEyeForModel modelMat camera) model vertexCountPerFace
  refillBuffer altVertexBufferId newVertice

  -- updating geometries can be long, update realTime after
  now <- get time
  return state { realTime = realToFrac now, simTime = 0, seed = newSeed }


updateState :: GlobalState -> CameraState -> IO (GlobalState)
updateState state@GlobalState{..} newCamera = do
  t <- get time
  let now = realToFrac t
  let dt = now - realTime
  case (leftButton newCamera, leftButton camera) of
    -- sim paused, do nothing
    (Press, _)         -> return state { camera = newCamera, realTime = now }
    -- sim runs, but max sim time is pi, so that interpolation stops matching the alt vertice
    (Release, Release) -> return state { camera = newCamera, realTime = now, simTime = min pi (simTime + dt) }
    -- sim restarted
    (Release, Press)   -> triggerReshape state { camera = newCamera }


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
  let (uR, dR, lR, rR) = (released u up, released d down, released l left, released r right)

  -- rotate model matrix when arrow key released
  let newMat0 = case (uR, dR) of
                 (False, False) -> modelMatrix
                 (True, _) -> multMat negXRot modelMatrix
                 (False, True) -> multMat posXRot modelMatrix

  let newMat1 = case (lR, rR) of
                 (False, False) -> newMat0
                 (True, _) -> multMat negYRot newMat0
                 (False, True) -> multMat posYRot newMat0

  let newKS = ks { up = u, down = d, left = l, right = r }

  -- if a rotation was applied, trigger reshape, else just update state
  if uR || dR || lR || rR
    then triggerReshape state { keys = newKS, modelMat = newMat1 }
    else return state { keys = newKS, modelMat = newMat1 }

  where released newK oldK = newK == Release && newK /= oldK


loop :: Bool -> IO Action -> GlobalState -> IO ()
loop static action state = do

  -- read mouse actions
  Action (action', camUpdater) <- action

  -- read keyboard actions
  newState0 <- handleKeys state

  -- update state with mouse actions
  let camState0 = camUpdater $ camera newState0

  -- apply zoom
  let k = (zoom $ camera newState0) / (zoom camState0)
  if (k /= 1)
    then do
      let projRef = projMat $ camera newState0
      oldProjection <- get projRef
      projRef $= Geometry.scale k oldProjection
    else return ()

  -- update camera
  let (Point3f px py pz) = camToPoint3f camState0
  let camState1 = camState0 { viewMat = lookAtMatrix (vec3 px py pz)
                                                     (vec3 0 0 0)
                                                     (vec3 0 1 0)
                            }

  -- check for sim restart
  newState <- if static
                then return newState0 { camera = camState1 }
                else updateState newState0 camState1

  -- render
  projection <- get $ projMat camState1

  let mvp = projection `multMat` (viewMat camState1) `multMat` (modelMat newState)
  render (simTime newState) mvp (glids newState)

  -- exit if window closed or Esc pressed
  esc <- GLFW.getKey GLFW.ESC
  q <- GLFW.getKey 'Q'
  open <- GLFW.getParam GLFW.Opened
  if open && esc /= GLFW.Press && q /= GLFW.Press
    then loop static action' newState
    else return ()


boolArgument :: String -> [String] -> Bool
boolArgument arg args = [] /= filter (\s -> s == arg) args


parseJsonArgs :: String -> (FilePath, [Int])
parseJsonArgs args = (head split, map read $ tail split)
  where split = splitOn "," args


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

  -- model from json?
  json <- readJson jsonFile

  let rawModel@(Model vs fs _) = maybe pentagonalHexecontahedron
                                     (parseJson indice)
                                     json

  let span = maximum $ map norm vs

  let model = FlatModel (facesToFlatTriangles vs fs) (facesToFlatIndice fs)
  let vertexCountPerFace = map ((1 +) . length) fs -- barycenter added to original faces
  let centersBuffer = centerBufferData fs
  let indiceBuffer = indexBufferData fs


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
  let camState = (camStateWithMatrice defaultCamState projMat) { distance = span * 1.1 }

  let bufferMaker = if static then (\ m _ -> (vertice m, defaultSeed))
                              else let camEye = camEyeForModel identity camState in
                                   (\ m s -> rndVertexBufferData s camEye m vertexCountPerFace)
  let (vertexBufferData, seed0) = bufferMaker model defaultSeed

  glids <- initGL (bothFaces || not invertFaces) (bothFaces || invertFaces) vertexBufferData indiceBuffer centersBuffer

  -- init global state
  now <- get time
  let state = GlobalState { camera = camState
                          , model = model
                          , seed = seed0
                          , glids = glids
                          , realTime = 0
                          , simTime = 0
                          , keys = defaultKeyState
                          , modelMat = identity
                          , vertexCountPerFace = vertexCountPerFace -- barycenter added to original faces
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