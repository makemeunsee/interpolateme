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
import Foreign.C.Types (CFloat)
import Data.Maybe (listToMaybe)
import Data.IORef (IORef, newIORef)
import Data.Vec (Mat44, identity)
import Data.List.Split (splitOn)
import qualified Random.MWC.Pure as RND

import FloretSphere
import Geometry ( Point3f (Point3f)
                , Model (Model)
                , normalized, cross, times, norm
                , scale
                , lookAtMatrix
                , orthoMatrix
                , multMat, multInvMatV
                , negXRot, posXRot, negYRot, posYRot
                , vec3, vec4, vec4ToPoint3f
                )
import FlatModel ( facesToFlatTriangles
                 , facesToCenterFlags
                 , facesToFlatIndice
                 , normalsToFlatNormals
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


data Action = Action (IO Action, MouseState -> MouseState)


data FlatModel = FlatModel { vertice :: [GLfloat], faces :: [Int], normals :: [GLfloat] }


data KeyState = KeyState { up :: KeyButtonState
                         , down :: KeyButtonState
                         , left :: KeyButtonState
                         , right :: KeyButtonState
                         , pgup :: KeyButtonState
                         , pgdn :: KeyButtonState
                         }


data OrbitingState = OrbitingState { theta :: GLfloat    -- angle between x axis and the orbiting point projected on the xz plane
                                   , phi :: GLfloat      -- angle between y axis and the orbiting point
                                   , distance :: GLfloat -- orbiting distance
                                   , viewMat :: Mat44f
                                   }


data MouseState = MouseState { mouseX :: GLint
                             , mouseY :: GLint
                             , wheel :: Int
                             , leftButton :: KeyButtonState
                             }
                  deriving Show


data GlobalState = GlobalState { camera :: OrbitingState
--                               , light :: OrbitingState
                               , mouse :: MouseState
                               , zoom :: GLfloat     -- scale for the model
                               , modelMat :: Mat44f
                               , model :: FlatModel
                               , seed :: RND.Seed
                               , glids :: GLIDs
                               , realTime :: GLfloat
                               , simTime :: GLfloat
                               , lightIntensity :: GLfloat
                               , keys :: KeyState
                               , projMat :: IORef Mat44f
                               , vertexCountPerFace :: [Int]
                               }


scaledModelMat :: GlobalState -> Mat44f
scaledModelMat global = Geometry.scale (zoom global) identity `multMat` (modelMat global)


-- state inits


defaultKeyState :: KeyState
defaultKeyState = KeyState Release Release Release Release Release Release


defaultMouseState :: MouseState
defaultMouseState = MouseState { mouseX = 0
                               , mouseY = 0
                               , wheel = 0
                               , leftButton = Release
                               }


defaultCamState :: OrbitingState
defaultCamState = OrbitingState { theta = pi/2
                                , phi = pi/2
                                , distance = 50
                                , viewMat = identity
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
                   , normalBufferId :: GLuint
                   , normalAttrib :: GLuint
                   , altVertexBufferId :: GLuint
                   , altVertexAttrib :: GLuint
                   , centerBufferId :: GLuint
                   , centerAttrib :: GLuint
                   }


initGL :: Bool -> Bool -> [GLfloat] -> [GLfloat] -> [GLuint] -> [GLfloat] -> IO GLIDs
initGL drawFront drawBack verticeData normalData indiceData centersData = do
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
  (AttribLocation normalAttrib) <- get (attribLocation prog "normal")
  (AttribLocation altVertexAttrib) <- get (attribLocation prog "alt_position")
  (AttribLocation centerAttrib) <- get (attribLocation prog "a_barycentric")
  indice <- makeBuffer ElementArrayBuffer indiceData
  let indiceCount = indexCount indiceData

  -- initially, vertice and alt vertice are equal
  let vertexBuffersLength = length verticeData
  vertexBufferId <- fillNewBuffer verticeData
  normalBufferId <- fillNewBuffer normalData
  altVertexBufferId <- fillNewBuffer verticeData
  centerBufferId <- fillNewBuffer centersData

  return GLIDs{..}


cleanUpGLStuff :: GLIDs -> IO ()
cleanUpGLStuff GLIDs{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  with normalBufferId $ glDeleteBuffers 1
  with altVertexBufferId $ glDeleteBuffers 1
  with centerBufferId $ glDeleteBuffers 1


-- rendering code


bindGeometry :: GLIDs -> IO ()
bindGeometry GLIDs{..} = do bindFloatBufferToAttrib 3 vertexBufferId vertexAttrib
                            bindFloatBufferToAttrib 3 normalBufferId normalAttrib
                            bindFloatBufferToAttrib 3 altVertexBufferId altVertexAttrib
                            bindFloatBufferToAttrib 1 centerBufferId centerAttrib


unbindGeometry :: GLIDs -> IO ()
unbindGeometry GLIDs{..} = do glDisableVertexAttribArray vertexAttrib
                              glDisableVertexAttribArray normalAttrib
                              glDisableVertexAttribArray altVertexAttrib
                              glDisableVertexAttribArray centerAttrib


bindUniformMatrix :: Program -> String -> Mat44f -> IO ()
bindUniformMatrix prog uName mat = do
  (UniformLocation mvpLoc) <- get $ uniformLocation prog uName
  with mat $ glUniformMatrix4fv mvpLoc 1 (fromBool True) . castPtr


render :: GLfloat -> GLfloat -> Mat44f -> Mat44f -> GLIDs -> IO ()
render t lightIntensity pMat mvpMat glids@GLIDs{..} = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  currentProgram $= Just prog

  -- bind matrices
  bindUniformMatrix prog "u_mvpMat" mvpMat
  bindUniformMatrix prog "u_pMat" pMat

  -- bind uniform colors
  colLoc <- get $ uniformLocation prog "u_color"
  bColLoc <- get $ uniformLocation prog "u_borderColor"
  uniform colLoc $= Color4 1 1 1 (1 :: GLfloat)
  uniform bColLoc $= Color4 0.4 0.4 0.4 (1 :: GLfloat)

  -- bind time
  timeLoc <- get $ uniformLocation prog "u_time"
  uniform timeLoc $= Index1 t

  -- bind light intensity
  lightLoc <- get $ uniformLocation prog "u_lightIntensity"
  uniform lightLoc $= Index1 lightIntensity

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


updateViewMat :: OrbitingState -> OrbitingState
updateViewMat orbit = orbit { viewMat = newViewMat }
  where (Point3f px py pz) = orbitingPosition orbit
        newViewMat = lookAtMatrix (vec3 px py pz)
                                  (vec3 0 0 0)
                                  (vec3 0 1 0)


-- direction to origin from orbiting position
orbitCenterDirection :: OrbitingState -> Point3GL
orbitCenterDirection orbit = (-1) `times` (orbitingPosition orbit)


-- orbiting point position
orbitingPosition :: OrbitingState -> Point3GL
orbitingPosition orbit = Point3f x y z
  where (x,y,z) = ( d * sin (phi orbit) * cos (theta orbit)
                  , d * cos (phi orbit)
                  , d * sin (phi orbit) * sin (theta orbit))
        d = distance orbit


orbitingEyeForModel :: Mat44f -> OrbitingState -> Point3GL
orbitingEyeForModel modelMatrix orbit = vec4ToPoint3f orbitEye
  where orbitEye = multInvMatV modelMatrix $ vec4 ex ey ez
        Point3f ex ey ez = orbitCenterDirection orbit


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


updateAngles :: GLint -> GLint -> OrbitingState -> OrbitingState
updateAngles diffX diffY orbit = orbit { theta = (theta orbit) + (fromIntegral diffX) * mouseSpeed
                                       , phi = limitAngle $ (phi orbit) + (fromIntegral diffY) * mouseSpeed
                                       }


updateCam :: MouseState -> MouseState -> OrbitingState -> OrbitingState
updateCam oldMouse newMouse orbit =
  if leftButton newMouse == Press
    then updateAngles diffX diffY orbit
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


mouseSpeed :: Floating a => a
mouseSpeed = 0.005


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
  let GLIDs{..} = glids

  -- read current vertex buffers
  oldVertice <- Main.readBuffer vertexBufferId vertexBuffersLength
  oldAltVertice <- Main.readBuffer altVertexBufferId vertexBuffersLength

  -- vertex buffer becomes interpolation (as of now) of vertice and alt vertice
  refillBuffer vertexBufferId $ interpolate simTime oldVertice oldAltVertice

  -- create new alt vertice ligned with the cam
  let (newVertice, newSeed) = rndVertexBufferData seed (orbitingEyeForModel (scaledModelMat state) camera) model vertexCountPerFace
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
  let lIntensity = lightIntensity state
  let ks@KeyState{..} = keys state

  -- read key inputs
  u <- GLFW.getKey GLFW.UP
  d <- GLFW.getKey GLFW.DOWN
  l <- GLFW.getKey GLFW.LEFT
  r <- GLFW.getKey GLFW.RIGHT
  pu <- GLFW.getKey GLFW.PAGEUP
  pd <- GLFW.getKey GLFW.PAGEDOWN

  let (uR, dR, lR, rR, puR, pdR) = ( released u up
                                   , released d down
                                   , released l left
                                   , released r right
                                   , released pu pgup
                                   , released pd pgdn
                                   )

  -- rotate model matrix when arrow key released
  let newMat0 = case (uR, dR) of
                 (False, False) -> modelMatrix
                 (True, _) -> multMat negXRot modelMatrix
                 (False, True) -> multMat posXRot modelMatrix

  let newMat1 = case (lR, rR) of
                 (False, False) -> newMat0
                 (True, _) -> multMat negYRot newMat0
                 (False, True) -> multMat posYRot newMat0

  let newKS = ks { up = u, down = d, left = l, right = r, pgup = pu, pgdn = pd }

  let newState0 = if puR
                    then state { lightIntensity = lIntensity * 1.2 }
                    else if pdR
                      then state { lightIntensity = lIntensity / 1.2 }
                      else state

  -- if a rotation was applied, trigger reshape, else just update state
  if uR || dR || lR || rR
    then triggerReshape newState0 { keys = newKS, modelMat = newMat1 }
    else return newState0 { keys = newKS, modelMat = newMat1 }

  where released newK oldK = newK == Release && newK /= oldK


updateView :: MouseState -> GlobalState -> GlobalState
updateView newMouseState global =
  newGlobal { camera = newCamera }
  where
    oldMouse = mouse global
    -- get new cam state from mouse actions
    newCamera0 = updateCam oldMouse newMouseState (camera global)

    -- apply zoom
    newGlobal = updateZoom oldMouse newMouseState global

    -- update view matrix in camera
    newCamera = updateViewMat newCamera0


loop :: Bool -> IO Action -> GlobalState -> IO ()
loop static action global = do

  -- read keyboard actions & update global state
  newGlobal0 <- handleKeys global

  -- read mouse actions
  Action (action', mouseStateUpdater) <- action

  -- get new mouse state from mouse actions
  let newMouseState = mouseStateUpdater $ mouse newGlobal0

  -- update the view related properties in the global state
  let newGlobal1 = updateView newMouseState newGlobal0

  -- check for sim restart, update sim & gl states if necessary
  newGlobal <- if static
                 then return newGlobal1 { mouse = newMouseState }
                 else updateSimState newMouseState newGlobal1

  -- prepare matrices for rendering
  projection <- get $ projMat newGlobal
  let view = viewMat $ camera newGlobal
  let vp = projection `multMat` view
  let mvp = vp `multMat` (scaledModelMat newGlobal)

  -- render
  render (simTime newGlobal) (lightIntensity newGlobal) projection mvp (glids newGlobal)

  -- exit if window closed or Esc pressed
  esc <- GLFW.getKey GLFW.ESC
  q <- GLFW.getKey 'Q'
  open <- GLFW.getParam GLFW.Opened
  if open && esc /= GLFW.Press && q /= GLFW.Press
    then loop static action' newGlobal
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

  let rawModel@(Model vs fs ns) = maybe snubRhombiMix
                                        (parseJson indice)
                                        json

  putStr "Model data summary - vertex count: "
  putStr $ show $ length vs
  putStr ", face count: "
  putStrLn $ show $ length fs

  let span = maximum $ map norm vs
  let model = FlatModel (facesToFlatTriangles vs fs) (facesToFlatIndice fs) (normalsToFlatNormals ns fs)

  putStr "'Enhanced' model data summary - vertex count: "
  putStrLn $ show $ length $ vertice model

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
  let camState = updateViewMat $ defaultCamState { distance = span * 1.1 }

  let bufferMaker = if static then (\ m _ -> (vertice m, defaultSeed))
                              else let camEye = orbitingEyeForModel identity camState in
                                   (\ m s -> rndVertexBufferData s camEye m vertexCountPerFace)
  let (vertexBufferData, seed0) = bufferMaker model defaultSeed

  glids <- initGL (bothFaces || not invertFaces)
                  (bothFaces || invertFaces)
                  vertexBufferData
                  (normals model)
                  indiceBuffer
                  centersBuffer

  -- init global state
  now <- get time
  projMat <- newIORef identity
  let state = GlobalState { camera = camState
                          , model = model
                          , mouse = defaultMouseState
                          , seed = seed0
                          , glids = glids
                          , realTime = 0
                          , simTime = 0
                          , lightIntensity = 15
                          , keys = defaultKeyState
                          , projMat = projMat
                          , modelMat = identity
                          , zoom = 1
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