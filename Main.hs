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
import Data.List (findIndices)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Random.MWC.Pure as RND

import FloretSphere
import qualified Geometry as G
import GLGenericFunctions ( naiveRotMat
                          , latLongPosition
                          , viewMatOf
                          )
import FlatModel ( FlatModel (FlatModel)
                 , fromModel
                 )
import ListUtil
import qualified PlaneCut as PC

-- NearZero instance for GLfloat
import Data.Vec.LinAlg (NearZero(..))
instance NearZero CFloat where
  nearZero x = abs x < 1e-6
  {-# INLINE nearZero #-}


type Vec4f = Vec4 GLfloat
type Mat44f = Mat44 GLfloat
type Point3GL = G.Point3f GLfloat
type FlatModelGL = FlatModel GLfloat GLuint


data Action = Action (IO Action, MouseState -> MouseState)


data KeyState = KeyState { tab :: KeyButtonState
                         , space :: KeyButtonState
                         , n :: KeyButtonState
                         }


data MouseState = MouseState { mouseX :: GLint
                             , mouseY :: GLint
                             , wheel :: Int
                             , leftButton :: KeyButtonState
                             }
                  deriving Show


data GlobalState = GlobalState { viewMat :: Mat44f
                               , showCutPlane :: Bool
                               , drawNormals :: Bool
                               , mouse :: MouseState
                               , zoom :: GLfloat     -- scale for the model
                               , modelMat :: Mat44f
                               , model :: PC.FacedModel GLfloat
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


-- rnd / num distribution functions


seedForString :: String -> RND.Seed
seedForString str = RND.seed $ map charToWord32 str
  where charToWord32 c = fromIntegral $ fromEnum c


-- GLfloat RangeRandom instance
instance RND.RangeRandom CFloat where
  range_random (x0, x1) s = (realToFrac r, s')
    where (r, s') = RND.range_random(realToFrac x0 :: Float, realToFrac x1 :: Float) s


-- GLint RangeRandom instance
instance RND.RangeRandom CInt where
  range_random (x0, x1) s = (fromIntegral r, s')
    where (r, s') = RND.range_random(fromIntegral x0 :: Int, fromIntegral x1 :: Int) s


rndSpherePosition :: (RealFloat a, RND.RangeRandom a) => RND.Seed -> (a, a, RND.Seed)
rndSpherePosition seed = (2*pi*u, acos $ 2*v - 1, newSeed)
  where
    (u, newSeed0) = RND.range_random (0, 1) seed
    (v, newSeed) = RND.range_random (0, 1) newSeed0


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
                               , ext :: Maybe ExtGeomInfo
                               }


data ExtGeomInfo = ExtGeomInfo { normalBufferId :: GLuint
                               , centerBufferId :: GLuint
                               }


data GLIDs = GLIDs { shaderInfo :: ShaderInfo
                   , objectBuffersInfo :: BuffersInfo
                   , planeBuffersInfo :: BuffersInfo
                   , normalsBuffersInfo :: BuffersInfo
                   }


createShader :: FilePath -> FilePath -> IO ShaderInfo
createShader vertexShaderFile fragShaderFile = do
  prog <- loadProgram vertexShaderFile fragShaderFile
  AttribLocation vertexAttrib <- get (attribLocation prog "position")
  AttribLocation normalAttrib <- get (attribLocation prog "normal")
  AttribLocation centerAttrib <- get (attribLocation prog "a_barycentrics")
  return ShaderInfo{..}


loadBuffers :: [GLfloat] -> Maybe [GLfloat] -> [GLuint] -> Maybe [GLfloat] -> IO BuffersInfo
loadBuffers verticeData m_normalData indiceData m_centersData = do

  indice <- makeBuffer ElementArrayBuffer indiceData
  let indiceCount = indexCount indiceData

  let vertexBuffersLength = length verticeData
  vertexBufferId <- fillNewFloatBuffer verticeData

  ext <- case (m_normalData, m_centersData) of
    (Just normalData, Just centersData) -> do
      normalBufferId <- fillNewFloatBuffer normalData
      centerBufferId <- fillNewFloatBuffer centersData
      return $ Just ExtGeomInfo{..}
    _ ->
      return Nothing

  return BuffersInfo{..}


initGL :: Bool -> Bool -> IO GLIDs
initGL drawFront drawBack = do
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
  shaderInfo <- Main.createShader "border_nolight.vert" "border_nolight.frag"

  -- create data buffers
  objectBuffersInfo <- loadBuffers [] Nothing [] Nothing
  normalsBuffersInfo <- loadBuffers [] Nothing [] Nothing
  planeBuffersInfo <- loadBuffers [ 0, 0, 1
                                  , 2, -2, 1
                                  , 2, 2, 1
                                  , -2, 2, 1
                                  , -2, -2, 1
                                  ]
                                  (Just $ take (3*5) $ cycle [ 1, 0, 0 ])
                                  [ 0, 1, 2
                                  , 0, 2, 3
                                  , 0, 3, 4
                                  , 0, 4, 1
                                  ]
                                  (Just
                                  [ 1, 1, 1
                                  , 1, 0, 0
                                  , 0, 1, 0
                                  , 1, 0, 0
                                  , 0, 1, 0] )

  return GLIDs{..}


cleanBuffers :: BuffersInfo -> IO ()
cleanBuffers BuffersInfo{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  case ext of
    Just ExtGeomInfo{..} -> do
      with normalBufferId $ glDeleteBuffers 1
      with centerBufferId $ glDeleteBuffers 1
    _ -> return ()


-- rendering code


bindSimpleGeometry :: ShaderInfo -> BuffersInfo -> IO ()
bindSimpleGeometry ShaderInfo{..} BuffersInfo{..} = do
  bindFloatBufferToAttrib 3 vertexBufferId vertexAttrib


bindGeometry :: ShaderInfo -> BuffersInfo -> IO ()
bindGeometry ShaderInfo{..} BuffersInfo{..} = do
  bindFloatBufferToAttrib 3 vertexBufferId vertexAttrib
  case ext of
    Just ExtGeomInfo{..} -> do
      bindFloatBufferToAttrib 3 normalBufferId normalAttrib
      bindFloatBufferToAttrib 3 centerBufferId centerAttrib
    _ -> return ()


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


render :: GLfloat -> Bool -> Bool -> Mat44f -> Mat44f -> GLIDs -> IO ()
render t drawPlane drawNormals mvp planeMvp glids@GLIDs{..} = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  let ShaderInfo{..} = shaderInfo

  currentProgram $= Just prog

  colLoc <- get $ uniformLocation prog "u_color"
  bColLoc <- get $ uniformLocation prog "u_borderColor"
  borderWidthLoc <- get $ uniformLocation prog "u_borderWidth"

  -- bind time
  timeLoc <- get $ uniformLocation prog "u_time"
  uniform timeLoc $= GL.Index1 t

  -- bind matrices
  bindUniformMatrix prog "u_mvpMat" mvp

  if drawNormals
    then do
      uniform colLoc $= GL.Color4 0 1 0 (1 :: GLfloat)
      uniform bColLoc $= GL.Color4 0 1 0 (1 :: GLfloat)

      uniform borderWidthLoc $= GL.Index1 (0 :: GLfloat)

      -- bind attributes
      bindGeometry shaderInfo normalsBuffersInfo

      -- bind indice
      bindBuffer ElementArrayBuffer $= Just (indice normalsBuffersInfo)

      drawElements GL.Lines (indiceCount normalsBuffersInfo) GL.UnsignedInt offset0

      unbindGeometry shaderInfo
    else return ()

  -- draw object

  -- bind uniform colors
  uniform colLoc $= GL.Color4 1 1 1 (1 :: GLfloat)
  uniform bColLoc $= GL.Color4 0.4 0.4 0.4 (1 :: GLfloat)

  uniform borderWidthLoc $= GL.Index1 (1.2 :: GLfloat)

  -- bind attributes
  bindGeometry shaderInfo objectBuffersInfo

  -- bind indice
  bindBuffer ElementArrayBuffer $= Just (indice objectBuffersInfo)

  drawElements GL.Triangles (indiceCount objectBuffersInfo) GL.UnsignedInt offset0

  unbindGeometry shaderInfo

  -- draw cut plane

  if drawPlane
    then do
      -- bind matrices
      bindUniformMatrix prog "u_mvpMat" planeMvp

      -- bind uniform colors
      colLoc <- get $ uniformLocation prog "u_color"
      bColLoc <- get $ uniformLocation prog "u_borderColor"
      uniform colLoc $= GL.Color4 0.8 0.3 0.3 (0.5 :: GLfloat)
      uniform bColLoc $= GL.Color4 1 0 0 (1 :: GLfloat)

      borderWidthLoc <- get $ uniformLocation prog "u_borderWidth"
      uniform borderWidthLoc $= GL.Index1 (5 :: GLfloat)

      -- bind attributes
      bindGeometry shaderInfo planeBuffersInfo

      -- bind indice
      bindBuffer ElementArrayBuffer $= Just (indice planeBuffersInfo)

      drawElements GL.Triangles (indiceCount planeBuffersInfo) GL.UnsignedInt offset0

      unbindGeometry shaderInfo
    else
      return ()

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


bindIntBufferToAttrib :: GLint -> GLuint -> GLuint -> IO ()
bindIntBufferToAttrib components bufId attribLoc = do
  glEnableVertexAttribArray attribLoc
  glBindBuffer gl_ARRAY_BUFFER bufId
  glVertexAttribPointer attribLoc -- attribute location in the shader
                        components -- components per vertex
                        gl_FLOAT -- coordinates type
                        (fromBool False) -- normalize?
                        (fromIntegral $ sizeOf (undefined::GLint) * fromIntegral components) -- stride
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


updateZoom :: MouseState -> MouseState -> GlobalState -> GlobalState
updateZoom oldMouse newMouse global = global { zoom = newZoom }
  where newZoom = max (min (zoom0*a) 16) 0.0625
        zoom0 = zoom global
        a = 1.01 ** fromIntegral wheelDiff
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
  kn <- GLFW.getKey 'N'

  let (spR, tbR, nR) = ( released sp space
                       , released tb tab
                       , released kn n
                       )

  newState0 <- if spR
    then do
      let m = model state
      -- origin point of the cutting plane:
      -- plane is static, with normal 0 0 1 and seed 0 0 1
      -- apply inverse of model matrix to find its position relative to the model
      let seed = G.vec4ToPoint3f $ G.multInvMatV (scaledModelMat state) $ G.vec4 0 0 1
      -- normal of the plane
      let G.Point3f kx ky kz = G.normalized seed
      let plane = PC.Plane kx ky kz seed
      let cut = PC.cutModel 0.00001 plane $ model state
--      putStrLn ""
--      putStrLn $ show cut
      loadModel state cut
    else
      return state

  let newKS = ks { tab = tb
                 , space = sp
                 , n = kn
                 }

  return newState0 { keys = newKS
                   , drawNormals = if nR then not $ drawNormals newState0 else drawNormals newState0
                   , showCutPlane = if tbR then not $ showCutPlane newState0 else showCutPlane newState0
                   }

  where released newK oldK = newK == Release && newK /= oldK


updateModelRot :: MouseState -> GlobalState -> GlobalState
updateModelRot newMouseState global =
  if leftButton newMouseState == Press then
    newGlobal { modelMat = naiveRotMat (diffX * 0.005) (diffY * 0.005) `G.multMat` modelMat global }
  else
    newGlobal
  where
    oldMouse = mouse global

    diffX = fromIntegral $ (mouseX newMouseState) - (mouseX oldMouse)
    diffY = fromIntegral $ (mouseY newMouseState) - (mouseY oldMouse)

    -- apply zoom
    newGlobal = updateZoom oldMouse newMouseState global


loop :: IO Action -> GlobalState -> IO GlobalState
loop action global = do

  -- read keyboard actions & update global state
  newGlobal0 <- handleKeys global

  -- read mouse actions
  Action (action', mouseStateUpdater) <- action

  -- get new mouse state from mouse actions
  let newMouseState = mouseStateUpdater $ mouse newGlobal0

  -- update the view related properties in the global state
  let newGlobal1 = updateModelRot newMouseState newGlobal0

  let newGlobal = newGlobal1 { mouse = newMouseState }

  -- prepare matrices for rendering
  p <- get $ projMat newGlobal
  let vp = p `G.multMat` viewMat newGlobal
  let mvp = vp `G.multMat` (scaledModelMat newGlobal)

  let planeMvp = vp

  -- render
  render (simTime newGlobal) (showCutPlane newGlobal0) (drawNormals newGlobal0) mvp planeMvp (glids newGlobal)

  -- exit if window closed or Esc pressed
  esc <- GLFW.getKey GLFW.ESC
  q <- GLFW.getKey 'Q'
  open <- GLFW.getParam GLFW.Opened
  if open && esc /= GLFW.Press && q /= GLFW.Press
    then loop action' newGlobal
    else return newGlobal


boolArgument :: String -> [String] -> Bool
boolArgument arg args = [] /= filter (== arg) args


strArgument :: String -> [String] -> Maybe String
strArgument arg args = listToMaybe $ drop 1 $ dropWhile (/= arg) args


loadModel :: GlobalState -> PC.FacedModel GLfloat -> IO GlobalState
loadModel global@GlobalState{..} m = do
  let GLIDs{..} = glids

  let vs = map (\(p,_,_) -> p) $ PC.vertice m
  let fs = map (\(f,_) -> map (\i -> head $ findIndices (\(_,_,j) -> j == i) $ PC.vertice m) f) $ PC.faces m
  let ns = fst $ unzip $ PC.normals m
  -- load next model
  let FlatModel vs' ns' cs ids vpf span = fromModel $ G.Model vs fs ns

--  putStrLn "loaded"
--  putStr "vs': "
--  putStrLn $ show vs'
--  putStr "ns': "
--  putStrLn $ show ns'
--  putStr "cs: "
--  putStrLn $ show cs
--  putStr "ids: "
--  putStrLn $ show ids
--  putStr "vpf: "
--  putStrLn $ show vpf
--  putStr "span: "
--  putStrLn $ show span

  -- clean gl buffers and recreate them with proper data
  cleanBuffers objectBuffersInfo
  cleanBuffers normalsBuffersInfo

  let vertexBufferData = vs
  newBuffersInfo <- loadBuffers vs' (Just ns') ids (Just cs)
  let faceCenters = map (G.faceBarycenter vs) fs
  let verticeOfNormalsBuffer = concatMap (\(G.Point3f cx cy cz, G.Point3f nx ny nz) ->
                                         [cx, cy, cz, cx + span / 10 * nx, cy + span / 10 * ny, cz + span / 10 * nz])
                                         $ zip faceCenters ns
  newNormalsBuffersInfo <- loadBuffers verticeOfNormalsBuffer Nothing (take (length verticeOfNormalsBuffer) [0..]) Nothing
  let newGlids = glids { objectBuffersInfo = newBuffersInfo, normalsBuffersInfo = newNormalsBuffersInfo }

  return global { glids = newGlids, model = m }


applyCuts [] m = Just m
applyCuts ((theta, phi):t) m =
  if m == m' then
    Nothing
  else
    applyCuts t m'
  where m' = PC.cutModel 0.00001 (PC.Plane nx ny nz sfPt) m
        sfPt@(G.Point3f nx ny nz) = latLongPosition theta phi 1


generateRndCuts 0 seed = ([], seed)
generateRndCuts n seed
  | n <= 0    = ([], seed)
  | otherwise = ((theta, phi) : arr, seed'')
  where
    (arr, seed'') = generateRndCuts (n-1) seed'
    (theta, phi, seed') = rndSpherePosition seed


main :: IO ()
main = do

--  putStrLn $ show $

  args <- getArgs

  -- fullscreen flag
  let fullScreenRequest = boolArgument "--f" args

  -- draw back and front faces
  let bothFaces = boolArgument "--b" args

  -- seed input
  let seedStr = strArgument "--s" args
  let seed = seedForString $ maybe "elohim" id seedStr

  -- cuts input
  let cutsStr = strArgument "--c" args
  let cuts = maybe 150
                   (\str -> case reads str of
                     [] -> 150
                     [(i, _)] -> i)
                   cutsStr


  -- initialize early to have access to time
  GLFW.initialize

  let m0@(G.Model vs fs ns) = icosahedron
  -- scale vertice, so that the distance from face centers to origin is 1
  let center0 = G.faceBarycenter vs $ fs !! 0
  let scale = map (G.divBy $ G.norm center0)
  let vs' = scale vs
  let cuttableModel = PC.FacedModel (map (\(i,(p,f)) -> (p,f,i)) $ zip [0..] $ zip vs' $ G.facesForEachVertex m0) (zip fs [0..]) (zip ns [0..])

  t0 <- get time
  let (rndCuts, _) = generateRndCuts cuts seed
  let rndCutsModel = fromJust $ applyCuts rndCuts cuttableModel
  putStrLn $ show $ length $ show rndCutsModel
  t1 <- get time
  putStr "Truncation duration: "
  putStrLn $ show (t1 - t0)

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

  -- init global state
  projMat <- newIORef identity
  let state0 = GlobalState { viewMat = viewMatOf (pi/2) (pi/2) 1
                           , showCutPlane = True
                           , drawNormals = False
                           , mouse = defaultMouseState
                           , glids = glstuff
                           , simTime = 0
                           , keys = defaultKeyState
                           , projMat = projMat
                           , modelMat = identity
                           , zoom = 1
                           }

  state <- loadModel state0 rndCutsModel

  -- setup stuff
  GLFW.swapInterval       $= 1 -- vsync
  GLFW.windowTitle        $= "Voronyi maze"
  GLFW.windowSizeCallback $= resize projMat
  -- main loop
  lastState <- loop waitForPress state
  -- exit
  cleanBuffers $ objectBuffersInfo $ glids lastState
  cleanBuffers $ planeBuffersInfo $ glids lastState
  cleanBuffers $ normalsBuffersInfo $ glids lastState
  GLFW.closeWindow
  GLFW.terminate