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
import Control.Monad (foldM)

import Foreign (with, fromBool, castPtr, alloca, peek, withArrayLen, sizeOf, Ptr, mallocArray, peekArray, free, nullPtr)
import Foreign.C.Types (CFloat, CInt)

import Data.Maybe (listToMaybe)
import Data.IORef (IORef, newIORef)
import Data.Vec (Mat44, Vec4, multmv, identity)
import Data.List (findIndices, elem, find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Map as M

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
import qualified VoronoiCut as VC
import qualified PlaneCut as PC
import Labyrinth

-- NearZero instance for GLfloat
import Data.Vec.LinAlg (NearZero(..))
instance NearZero CFloat where
  nearZero x = abs x < 1e-6
  {-# INLINE nearZero #-}


type Vec4f = Vec4 GLfloat
type Mat44f = Mat44 GLfloat
type Point3GL = G.Point3f GLfloat
type FlatModelGL = FlatModel GLfloat


data Action = Action (IO Action, MouseState -> MouseState)


data KeyState = KeyState { n :: KeyButtonState
                         , s :: KeyButtonState
                         , l :: KeyButtonState
                         , c :: KeyButtonState
                         , d :: KeyButtonState
                         }


data MouseState = MouseState { mouseX :: GLint
                             , mouseY :: GLint
                             , wheel :: Int
                             , leftButton :: KeyButtonState
                             }
                  deriving Show


data GlobalState = GlobalState { viewMat :: Mat44f
                               , drawSolid :: Bool
                               , drawNormals :: Bool
                               , altLaby :: Bool
                               , drawLabyrinth :: Bool
--                               , drawWalls :: Bool
                               , depthRender :: Bool
                               , mouse :: MouseState
                               , modelMat :: Mat44f
                               , model :: VC.VoronoiModel GLfloat
                               , glids :: GLIDs
                               , simTime :: GLfloat
                               , seed :: RND.Seed
                               , keys :: KeyState
                               , projMat :: IORef Mat44f
                               , zoom :: IORef GLfloat
                               , pendingCuts :: [(GLfloat, GLfloat)]
                               , lastCutTime :: Double
                               }


-- state inits


defaultKeyState :: KeyState
defaultKeyState = KeyState Release Release Release Release Release


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
                             , mazeAttrib :: GLuint
                             }


data BuffersInfo = BuffersInfo { indice :: BufferObject
                               , indiceCount :: GLint
                               , vertexBuffersLength :: Int
                               , vertexBufferId :: GLuint
                               , ext :: Maybe ExtGeomInfo
                               }


data ExtGeomInfo = ExtGeomInfo { normalBufferId :: GLuint
                               , centerBufferId :: GLuint
                               , mazeBufferId :: GLuint
                               }


data GLIDs = GLIDs { shaderInfo :: ShaderInfo
                   , objectBuffersInfo :: BuffersInfo
                   , labyrinthBuffersInfo :: BuffersInfo
--                   , wallsBuffersInfo :: BuffersInfo
                   , normalsBuffersInfo :: BuffersInfo
                   , bugMarkerBuffersInfo :: BuffersInfo
                   }


createShader :: FilePath -> FilePath -> IO ShaderInfo
createShader vertexShaderFile fragShaderFile = do
  prog <- loadProgram vertexShaderFile fragShaderFile
  AttribLocation vertexAttrib <- get (attribLocation prog "position")
  AttribLocation normalAttrib <- get (attribLocation prog "normal")
  AttribLocation centerAttrib <- get (attribLocation prog "a_centerFlag")
  AttribLocation mazeAttrib <- get (attribLocation prog "a_mazeDepth")
  return ShaderInfo{..}


loadBuffers :: [GLfloat] -> [GLuint] -> Maybe [GLfloat] -> Maybe [GLfloat] -> Maybe [GLfloat] -> IO BuffersInfo
loadBuffers verticeData indiceData m_normalData m_centersData m_mazeData = do

  indice <- makeBuffer ElementArrayBuffer indiceData
  let indiceCount = indexCount indiceData

  let vertexBuffersLength = length verticeData
  vertexBufferId <- fillNewFloatBuffer verticeData

  ext <- case (m_normalData, m_centersData, m_mazeData) of
    (Just normalData, Just centersData, Just mazeData) -> do
      normalBufferId <- fillNewFloatBuffer normalData
      centerBufferId <- fillNewFloatBuffer centersData
      mazeBufferId <- fillNewFloatBuffer mazeData
      return $ Just ExtGeomInfo{..}
    _ ->
      return Nothing

  return BuffersInfo{..}


initGL :: Bool -> Bool -> Maybe (GLfloat, GLfloat) -> IO GLIDs
initGL drawFront drawBack m_angles = do
  GL.depthFunc $= Just GL.Less
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.shadeModel $= GL.Smooth

  let front = if drawFront then GL.Fill else GL.Line
  let back = if drawBack then GL.Fill else GL.Line
  GL.polygonMode $= (front, back)

  GL.cullFace $= Just GL.Back
--  GL.cullFace $= Nothing

  GL.lineWidth $= 1

  -- make shaders & data
  shaderInfo <- Main.createShader "border_nolight.vert" "border_nolight.frag"

  -- create data buffers
  objectBuffersInfo <- loadBuffers [] [] Nothing Nothing Nothing
  normalsBuffersInfo <- loadBuffers [] [] Nothing Nothing Nothing
  bugMarkerBuffersInfo <- case m_angles of
                            Nothing -> loadBuffers [] [] Nothing Nothing Nothing
                            Just (theta,phi) -> do
                              let G.Point3f x y z = latLongPosition theta phi 1
                              loadBuffers [x,y,z,x*2,y*2,z*2] [0,1] Nothing Nothing Nothing

  labyrinthBuffersInfo <- loadBuffers [] [] Nothing Nothing Nothing
--  wallsBuffersInfo <- loadBuffers [] [] Nothing Nothing Nothing

  return GLIDs{..}


cleanBuffers :: BuffersInfo -> IO ()
cleanBuffers BuffersInfo{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  case ext of
    Just ExtGeomInfo{..} -> do
      with normalBufferId $ glDeleteBuffers 1
      with centerBufferId $ glDeleteBuffers 1
      with mazeBufferId $ glDeleteBuffers 1
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
      bindFloatBufferToAttrib 1 centerBufferId centerAttrib
      bindFloatBufferToAttrib 1 mazeBufferId mazeAttrib
    _ -> return ()


unbindGeometry :: ShaderInfo -> IO ()
unbindGeometry ShaderInfo{..} = do
  glDisableVertexAttribArray vertexAttrib
  glDisableVertexAttribArray normalAttrib
  glDisableVertexAttribArray centerAttrib
  glDisableVertexAttribArray mazeAttrib


bindUniformMatrix :: Program -> String -> Mat44f -> IO ()
bindUniformMatrix prog uName mat = do
  (UniformLocation matLoc) <- get $ uniformLocation prog uName
  with mat $ glUniformMatrix4fv matLoc 1 (fromBool True) . castPtr


bindUniformVector :: Program -> String -> Vec4f -> IO ()
bindUniformVector prog uName vec = do
  (UniformLocation vLoc) <- get $ uniformLocation prog uName
  with vec $ glUniform4fv vLoc 1 . castPtr


render :: GLfloat -> Bool -> Bool -> Bool -> Bool -> Mat44f -> GLIDs -> IO ()
render t drawSolid drawNormals drawLabyrinth depthRender mvp glids@GLIDs{..} = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  let ShaderInfo{..} = shaderInfo

  currentProgram $= Just prog

  colLoc <- get $ uniformLocation prog "u_color"
  bColLoc <- get $ uniformLocation prog "u_borderColor"
  borderWidthLoc <- get $ uniformLocation prog "u_borderWidth"
  depthRenderLoc <- get $ uniformLocation prog "u_allowDepth"

  -- bind time
  timeLoc <- get $ uniformLocation prog "u_time"
  uniform timeLoc $= GL.Index1 t

  -- bind matrices
  bindUniformMatrix prog "u_mvpMat" mvp

  -- depth render mode
  uniform depthRenderLoc $= GL.Index1 (if depthRender then 1.0 else 0.0 :: GLfloat)

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
    else
      return ()


  -- bug marker
  uniform colLoc $= GL.Color4 0 0 0 (1 :: GLfloat)
  uniform bColLoc $= GL.Color4 0 0 0 (1 :: GLfloat)
  uniform borderWidthLoc $= GL.Index1 (0 :: GLfloat)
  -- bind attributes
  bindGeometry shaderInfo bugMarkerBuffersInfo
  -- bind indice
  bindBuffer ElementArrayBuffer $= Just (indice bugMarkerBuffersInfo)
  drawElements GL.Lines (indiceCount bugMarkerBuffersInfo) GL.UnsignedInt offset0
  unbindGeometry shaderInfo


  -- draw object
  -- bind uniform colors
  if drawSolid
    then do
      uniform colLoc $= GL.Color4 1 1 1 (1 :: GLfloat)
      uniform bColLoc $= GL.Color4 0.05 0.05 0.05 (1 :: GLfloat)
      uniform borderWidthLoc $= GL.Index1 (0.8 :: GLfloat)

      -- bind attributes
      bindGeometry shaderInfo objectBuffersInfo

      -- bind indice
      bindBuffer ElementArrayBuffer $= Just (indice objectBuffersInfo)

      drawElements GL.Triangles (indiceCount objectBuffersInfo) GL.UnsignedInt offset0

      unbindGeometry shaderInfo
    else do
--      uniform colLoc $= GL.Color4 0 0 0 (1 :: GLfloat)
--      uniform bColLoc $= GL.Color4 0 0 0 (1 :: GLfloat)
      return ()


--  -- draw walls
--  if drawWalls
--    then do
--      -- bind uniform colors
--      uniform colLoc $= GL.Color4 0 1 0 (1 :: GLfloat)
--      uniform bColLoc $= GL.Color4 0 1 0 (1 :: GLfloat)
--
--      uniform borderWidthLoc $= GL.Index1 (0 :: GLfloat)
--
--      -- bind attributes
--      bindGeometry shaderInfo wallsBuffersInfo
--
--      -- bind indice
--      bindBuffer ElementArrayBuffer $= Just (indice wallsBuffersInfo)
--
--      drawElements GL.Lines (indiceCount wallsBuffersInfo) GL.UnsignedInt offset0
--
--      unbindGeometry shaderInfo
--    else do
--      return ()


  -- draw labyrinth path
  if drawLabyrinth
    then do
      -- bind uniforms
      uniform colLoc $= GL.Color4 1 0 0 (1 :: GLfloat)
      uniform bColLoc $= GL.Color4 1 0 0 (1 :: GLfloat)
      uniform borderWidthLoc $= GL.Index1 (0 :: GLfloat)

      -- bind attributes
      bindGeometry shaderInfo labyrinthBuffersInfo

      -- bind indice
      bindBuffer ElementArrayBuffer $= Just (indice labyrinthBuffersInfo)

      drawElements GL.Lines (indiceCount labyrinthBuffersInfo) GL.UnsignedInt offset0

      unbindGeometry shaderInfo

      -- bind uniforms
      uniform colLoc $= GL.Color4 1 0 0 (1 :: GLfloat)
      uniform bColLoc $= GL.Color4 1 0 0 (1 :: GLfloat)
      uniform borderWidthLoc $= GL.Index1 (0 :: GLfloat)
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

resize :: IORef Mat44f -> IORef GLfloat -> GLFW.WindowSizeCallback
resize projMatRef zoomRef size@(GL.Size w h) = do
  zoom <- get zoomRef
  GL.viewport $= (GL.Position 0 0, size)
  projMatRef $= (G.scale zoom $ G.orthoMatrixFromScreen w h 2)
  return ()


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
  GL.Position x y <- GL.get GLFW.mousePos
  b <- GLFW.getMouseButton GLFW.ButtonLeft
  wheel <- get mouseWheel
  case b of
    -- when button is released, switch back back to
    -- waitForPress action
    GLFW.Release -> return  $ Action (waitForPress, applyMouseMove b x y wheel)
    GLFW.Press   -> return  $ Action (waitForRelease, applyMouseMove b x y wheel)


-- rotate model matrix on arrow keys released
handleKeys :: GlobalState -> IO GlobalState
handleKeys state = do
  let modelMatrix = modelMat state
  let keyState@KeyState{..} = keys state

  -- read key inputs
  tb <- GLFW.getKey GLFW.TAB
  sp <- GLFW.getKey ' '
  kn <- GLFW.getKey 'N'
  ks <- GLFW.getKey 'S'
  kl <- GLFW.getKey 'L'
  kc <- GLFW.getKey 'C'
  kw <- GLFW.getKey 'W'
  kd <- GLFW.getKey 'D'
  kpl <- GLFW.getKey GLFW.KP_ADD
  kmi <- GLFW.getKey GLFW.KP_SUBTRACT

  let nR = released kn n
  let lR = released kl l
  let sR = released ks s
  let cR = released kc c
  let dR = released kd d

  let newKeyState = keyState { n = kn
                             , s = ks
                             , l = kl
                             , c = kc
                             , d = kd
                             }

  let (newCuts, newSeed) = if cR then
                             let (newC, newS) = generateRndCuts 50 (seed state) in
                             (pendingCuts state ++ newC, newS)
                           else
                             (pendingCuts state, seed state)

  let zoomF = if kpl == Press then 1.01 else if kmi == Press then (1/1.01) else 1
  if zoomF /= 1
    then do
      m <- get $ projMat state
      z <- get $ zoom state
      zoom state $= zoomF * z
      projMat state $= G.scale zoomF m
    else
      return ()


  return state { keys = newKeyState
               , drawSolid = if sR then not $ drawSolid state else drawSolid state
               , drawNormals = if nR then not $ drawNormals state else drawNormals state
               , drawLabyrinth = if lR then not $ drawLabyrinth state else drawLabyrinth state
--               , drawWalls = if wR then not $ drawWalls state else drawWalls state
               , depthRender = if dR then not $ depthRender state else depthRender state
               , pendingCuts = newCuts
               , seed = newSeed
               }

  where released newK oldK = newK == Release && newK /= oldK


updateModelRot :: GLfloat -> MouseState -> GlobalState -> GlobalState
updateModelRot speed newMouseState global =
  if leftButton newMouseState == Press then
    global { modelMat = naiveRotMat (diffX * speed) (diffY * speed) `G.multMat` modelMat global }
  else
    global
  where

    oldMouse = mouse global

    diffX = fromIntegral $ (mouseX newMouseState) - (mouseX oldMouse)
    diffY = fromIntegral $ (mouseY newMouseState) - (mouseY oldMouse)


loop :: IO Action -> GlobalState -> IO GlobalState
loop action global = do

  -- read keyboard actions & update global state
  newGlobal0 <- handleKeys global
  z <- get $ zoom newGlobal0
  let speed = 1 / z / 50

  -- read mouse actions
  Action (action', mouseStateUpdater) <- action

  -- get new mouse state from mouse actions
  let newMouseState = mouseStateUpdater $ mouse newGlobal0

  -- update the view related properties in the global state
  let newGlobal1 = updateModelRot speed newMouseState newGlobal0

  let newGlobal2 = newGlobal1 { mouse = newMouseState }


  -- cut model
  t <- get time
  newGlobal <- case pendingCuts newGlobal2 of
                 [] -> return newGlobal2
                 (th,ph) : cs
                   | t - lastCutTime newGlobal2 > 0.01 -> do
                     let m' = applyCut th ph $ model newGlobal2
                     newState <- loadModel newGlobal2 m'
                     t' <- get time
                     return newState { pendingCuts = cs, lastCutTime = t' }
                   | otherwise -> return newGlobal2

  -- prepare matrices for rendering
  p <- get $ projMat newGlobal2
  let vp = p `G.multMat` viewMat newGlobal2
  let mvp = vp `G.multMat` modelMat newGlobal2

  -- render
  render (simTime newGlobal)
         (drawSolid newGlobal0)
         (drawNormals newGlobal0)
         (drawLabyrinth newGlobal0)
--         (drawWalls newGlobal0)
         (depthRender newGlobal0)
         mvp
         (glids newGlobal)

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


loadModel :: GlobalState -> VC.VoronoiModel GLfloat -> IO GlobalState
loadModel global@GlobalState{..} vm = do

  t0 <- get time
  let GLIDs{..} = glids

  let faces = VC.faces vm
  let fc = VC.faceCount vm

  t1 <- get time
  let laby2 = labyrinth2 faces
  let (laby1, seed') = labyrinth1 seed faces
  let laby = if altLaby then laby2 else laby1
  putStrLn $ "mazes sizes:\t" ++ show (size laby)
  t2 <- get time
  putStrLn $ "laby gen duration:\t" ++ show (t2 - t1)

--  let mazeBuffer = mazeData laby vm
  let (depths, maxDepth) = depthMap laby
  let (vertexBuffer, ids, centerBuffer, mazeBuffer, normalBuffer) = VC.toBufferData faces (M.toAscList depths) maxDepth

  -- clean gl buffers and recreate them with proper data
  cleanBuffers objectBuffersInfo
  cleanBuffers normalsBuffersInfo
  cleanBuffers labyrinthBuffersInfo
--  cleanBuffers wallsBuffersInfo

  newBuffersInfo <- loadBuffers (concatMap G.pointToArr vertexBuffer)
                                ids
                                (Just $ concatMap G.pointToArr normalBuffer)
                                (Just centerBuffer)
                                (Just mazeBuffer)

  let faceCenters = VC.normals vm -- on the unit sphere so they're normalized too
  let verticeOfNormalsBuffer = concatMap (\(G.Point3f cx cy cz) ->
                                         [cx, cy, cz, cx + 0.1 * cx, cy + 0.1 * cy, cz + 0.1 * cz])
                                         faceCenters
  newNormalsBuffersInfo <- loadBuffers verticeOfNormalsBuffer
                                       (take (length verticeOfNormalsBuffer) [0..])
                                       Nothing
                                       Nothing
                                       Nothing

  let pathVertice = labyrinthToPathVertice faces laby
  let pathIndice = labyrinthToPathIndice 0 laby
--  let wallVertice = labyrinthToWallVertice faces laby []
--  let (wallIndice, _) = labyrinthToWallIndice 0 (map VC.neighbours $ VC.faceList vm) laby

  newLabyrinthBuffersInfo <- loadBuffers (concatMap (\(G.Point3f x y z) -> [1.001*x,1.001*y,1.001*z]) pathVertice)
                                         pathIndice
                                         Nothing
                                         Nothing
                                         Nothing

--  newWallsBuffersInfo <- loadBuffers (concatMap (\(G.Point3f x y z) -> [1.001*x,1.001*y,1.001*z]) wallVertice)
--                                     wallIndice
--                                     Nothing
--                                     Nothing
--                                     Nothing

  t3 <- get time
  putStrLn $ "load model duration:\t" ++ show (t3 - t0)

  let newGlids = glids { objectBuffersInfo = newBuffersInfo
                       , normalsBuffersInfo = newNormalsBuffersInfo
                       , labyrinthBuffersInfo = newLabyrinthBuffersInfo
--                       , wallsBuffersInfo = newWallsBuffersInfo
                       }

  return global { glids = newGlids, model = vm, seed = seed' }


applyCut theta phi m = VC.cutModel m (PC.Plane nx ny nz sfPt)
  where sfPt@(G.Point3f nx ny nz) = latLongPosition theta phi 1


generateRndCuts 0 seed = ([], seed)
generateRndCuts n seed
  | n <= 0    = ([], seed)
  | otherwise = ((theta, phi) : arr, seed'')
  where
    (arr, seed'') = generateRndCuts (n-1) seed'
    (theta, phi, seed') = rndSpherePosition seed


main :: IO ()
main = do

  args <- getArgs

  -- fullscreen flag
  let fullScreenRequest = boolArgument "--f" args

  -- draw back and front faces
  let bothFaces = boolArgument "--b" args

  -- use alternative maze
  let altMaze = boolArgument "--a" args

  -- seed input
  let seedStr = maybe "imullinati" id $ strArgument "--s" args
  let seed = seedForString seedStr

  putStrLn $ "seed:\t" ++ seedStr

  let defaultCutCount = 1000
  -- cuts input
  let cutsStr = strArgument "--c" args
  let cuts = maybe defaultCutCount
                   (\str -> case reads str of
                     [] -> defaultCutCount
                     [(i, _)] -> i)
                   cutsStr


  -- initialize early to have access to time
  GLFW.initialize

  let cuttableModel = VC.fromModel icosahedron

  t0 <- get time
  let (rndCuts, seed') = generateRndCuts cuts seed
  putStrLn $ "cuts:\t" ++ show cuts
  let rndCutsModel = foldr (\(t,p) m  -> applyCut t p m) cuttableModel $ reverse rndCuts
  putStrLn $ "last face seed:\t" ++ (show $ VC.seed $ VC.lastFace rndCutsModel)
--  putStrLn "cut\tfaces\tduration"
--  (rndCutsModel, _) <- F.foldrM (\(t,p) (m,i)  -> do
--                                  putStr $ show i
--                                  t0 <- get time
--                                  let m' = applyCut t p m
--                                  putStr $ "\t" ++ (show $ VC.faceCount m') ++ "\t"
--                                  t1 <- get time
--                                  putStrLn $ show $ t1-t0
--                                  return (m', i+1)
--                                )
--                                (cuttableModel, 0)
--                                $ reverse rndCuts
  t1 <- get time
  putStrLn $ "topology:\t" ++ (show $ map VC.neighbours $ VC.faceList rndCutsModel)
  putStrLn $ "Truncation duration: " ++ show (t1 - t0)

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
                    Nothing

  -- init global state
  projMat <- newIORef identity
  zoom <- newIORef 1.75
  let state0 = GlobalState { viewMat = viewMatOf (pi/2) (pi/2) 1
                           , drawSolid = True
                           , drawNormals = False
                           , altLaby = altMaze
                           , drawLabyrinth = False
--                           , drawWalls = False
                           , depthRender = False
                           , mouse = defaultMouseState
                           , glids = glstuff
                           , simTime = 0
                           , seed = seed'
                           , keys = defaultKeyState
                           , projMat = projMat
                           , modelMat = identity
                           , zoom = zoom
                           , pendingCuts = []
                           , lastCutTime = 0
                           }

  state <- loadModel state0 rndCutsModel

  -- setup stuff
  GLFW.swapInterval       $= 1 -- vsync
  GLFW.windowTitle        $= "Voronyi maze"
  GLFW.windowSizeCallback $= resize projMat zoom
  -- main loop
  lastState <- loop waitForPress state
  -- exit
  cleanBuffers $ objectBuffersInfo $ glids lastState
  cleanBuffers $ normalsBuffersInfo $ glids lastState
  cleanBuffers $ bugMarkerBuffersInfo $ glids lastState
  cleanBuffers $ labyrinthBuffersInfo $ glids lastState
--  cleanBuffers $ wallsBuffersInfo $ glids lastState
  GLFW.closeWindow
  GLFW.terminate