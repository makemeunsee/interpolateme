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

import Control.Monad (foldM)

import Foreign (with, fromBool, castPtr, alloca, peek, withArrayLen, sizeOf, Ptr, mallocArray, peekArray, free, nullPtr)
import Foreign.C.Types (CFloat, CInt)

import Data.Maybe (listToMaybe)
import Data.IORef (IORef, newIORef)
import Data.Vec (Mat44, Vec4, multmv, identity)

import Models
import qualified Geometry as G
import qualified LinAlgFunctions as LAF
import qualified VoronoiCut as VC
import Labyrinth
import RandomUtil


type Vec4f = Vec4 GLfloat
type Mat44f = Mat44 GLfloat


data Action = Action (IO Action, MouseState -> MouseState)


data KeyState = KeyState { n :: KeyButtonState
                         , s :: KeyButtonState
                         , l :: KeyButtonState
                         , d :: KeyButtonState
                         , i :: KeyButtonState
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
                               , computeMazePath :: Bool
                               , drawMazePath :: Bool
                               , mazeDepthGap :: Int
                               , depthRender :: Bool
                               , depthInvert :: Bool
                               , depthScale :: GLfloat
                               , explodedFactor :: GLfloat
                               , mouse :: MouseState
                               , modelMat :: Mat44f
                               , glids :: GLIDs
                               , simTime :: GLfloat
                               , keys :: KeyState
                               , projMat :: IORef Mat44f
                               , zoom :: IORef GLfloat
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
                               , ext :: OptionGeomInfo
                               }


data OptionGeomInfo = OptionGeomInfo { normalBufferId :: Maybe GLuint
                                     , centerBufferId :: Maybe GLuint
                                     , mazeBufferId :: Maybe GLuint
                                     }


data GLIDs = GLIDs { shaderInfo :: ShaderInfo
                   , objectBuffersInfo :: BuffersInfo
                   , labyrinthBuffersInfo :: BuffersInfo
                   , normalsBuffersInfo :: BuffersInfo
                   }


createShader :: FilePath -> FilePath -> IO ShaderInfo
createShader vertexShaderFile fragShaderFile = do
  prog <- loadProgram vertexShaderFile fragShaderFile
  AttribLocation vertexAttrib <- get (attribLocation prog "position")
  AttribLocation normalAttrib <- get (attribLocation prog "normal")
  AttribLocation centerAttrib <- get (attribLocation prog "a_centerFlag")
  AttribLocation mazeAttrib <- get (attribLocation prog "a_mazeDepth")
  return ShaderInfo{..}


loadOptionalBuffers :: Maybe [GLfloat] -> Maybe [GLfloat] -> Maybe [GLfloat] -> IO OptionGeomInfo
loadOptionalBuffers m_normalData m_centersData m_mazeData = do
  normalBufferId <- loadOptionalBuffer m_normalData
  centerBufferId <- loadOptionalBuffer m_centersData
  mazeBufferId <- loadOptionalBuffer m_mazeData
  return OptionGeomInfo{..}


loadOptionalBuffer :: Maybe [GLfloat] -> IO (Maybe GLuint)
loadOptionalBuffer m_data = do case m_data of
                                 Nothing -> return Nothing
                                 Just dat -> do
                                             id <- fillNewFloatBuffer dat
                                             return $ Just id


loadBuffers :: [GLfloat] -> [GLuint] -> Maybe [GLfloat] -> Maybe [GLfloat] -> Maybe [GLfloat] -> IO BuffersInfo
loadBuffers verticeData indiceData m_normalData m_centersData m_mazeData = do

  indice <- makeBuffer ElementArrayBuffer indiceData
  let indiceCount = indexCount indiceData
  let vertexBuffersLength = length verticeData
  vertexBufferId <- fillNewFloatBuffer verticeData
  ext <- loadOptionalBuffers m_normalData m_centersData m_mazeData
  return BuffersInfo{..}


initGL :: IO GLIDs
initGL = do
  GL.depthFunc $= Just GL.Less
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.shadeModel $= GL.Smooth

  GL.cullFace $= Just GL.Back

  GL.lineWidth $= 1

  -- make shaders & data
  shaderInfo <- Main.createShader "border_nolight.vert" "border_nolight.frag"

  -- create data buffers
  objectBuffersInfo <- loadBuffers [] [] Nothing Nothing Nothing
  normalsBuffersInfo <- loadBuffers [] [] Nothing Nothing Nothing
  labyrinthBuffersInfo <- loadBuffers [] [] Nothing Nothing Nothing

  return GLIDs{..}


cleanBuffers :: BuffersInfo -> IO ()
cleanBuffers BuffersInfo{..} = do
  with vertexBufferId $ glDeleteBuffers 1
  cleanOptionalBuffers ext


cleanOptionalBuffers :: OptionGeomInfo -> IO ()
cleanOptionalBuffers OptionGeomInfo{..} = do
  cleanOptionalBuffer normalBufferId
  cleanOptionalBuffer centerBufferId
  cleanOptionalBuffer mazeBufferId


cleanOptionalBuffer :: Maybe GLuint -> IO ()
cleanOptionalBuffer m_id = do
  case m_id of Nothing -> return ()
               Just i  -> with i $ glDeleteBuffers 1


-- rendering code


bindSimpleGeometry :: ShaderInfo -> BuffersInfo -> IO ()
bindSimpleGeometry ShaderInfo{..} BuffersInfo{..} = do
  bindFloatBufferToAttrib 3 vertexBufferId vertexAttrib


bindGeometry :: ShaderInfo -> BuffersInfo -> IO ()
bindGeometry si@ShaderInfo{..} BuffersInfo{..} = do
  bindFloatBufferToAttrib 3 vertexBufferId vertexAttrib
  bindOptionalGeometries ext si


bindOptionalGeometries :: OptionGeomInfo -> ShaderInfo -> IO ()
bindOptionalGeometries OptionGeomInfo{..} ShaderInfo{..} = do
  bindOptionalGeometry 3 normalAttrib normalBufferId
  bindOptionalGeometry 1 centerAttrib centerBufferId
  bindOptionalGeometry 1 mazeAttrib mazeBufferId


bindOptionalGeometry :: GLint -> GLuint -> Maybe GLuint -> IO ()
bindOptionalGeometry size attribId m_id = do
  case m_id of Nothing -> return ()
               Just i  -> bindFloatBufferToAttrib size i attribId



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


render :: GLfloat -> Bool -> Bool -> Bool -> GLfloat -> GLfloat -> GLfloat -> Mat44f -> GLIDs -> IO ()
render t drawSolid drawNormals drawMazePath depthMode depthScale explodedFactor mvp glids@GLIDs{..} = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  let ShaderInfo{..} = shaderInfo

  currentProgram $= Just prog

  colLoc <- get $ uniformLocation prog "u_color"
  bColLoc <- get $ uniformLocation prog "u_borderColor"
  borderWidthLoc <- get $ uniformLocation prog "u_borderWidth"
  depthModeLoc <- get $ uniformLocation prog "u_depthMode"
  depthScaleLoc <- get $ uniformLocation prog "u_depthScale"
  expFactLoc <- get $ uniformLocation prog "u_explodedFactor"

  -- bind time
  timeLoc <- get $ uniformLocation prog "u_time"
  uniform timeLoc $= GL.Index1 t

  -- bind matrices
  bindUniformMatrix prog "u_mvpMat" mvp

  -- depth render mode
  uniform depthModeLoc $= GL.Index1 depthMode
  uniform depthScaleLoc $= GL.Index1 depthScale

  -- exploded
  uniform expFactLoc $= GL.Index1 (explodedFactor-1)

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


  -- draw object
  -- bind uniform colors
  if drawSolid
    then do
      uniform colLoc $= GL.Color4 1 1 1 (1 :: GLfloat)
      uniform bColLoc $= GL.Color4 0.05 0.05 0.05 (1 :: GLfloat)
      uniform borderWidthLoc $= GL.Index1 (1.0 :: GLfloat)

      -- bind attributes
      bindGeometry shaderInfo objectBuffersInfo

      -- bind indice
      bindBuffer ElementArrayBuffer $= Just (indice objectBuffersInfo)

      drawElements GL.Triangles (indiceCount objectBuffersInfo) GL.UnsignedInt offset0

      unbindGeometry shaderInfo
    else do
      return ()


  -- draw labyrinth path
  if drawMazePath
    then do
      -- bind uniforms
      uniform colLoc $= GL.Color4 0.4 0.4 0.4 (1 :: GLfloat)
      uniform bColLoc $= GL.Color4 0.4 0.4 0.4 (1 :: GLfloat)
      uniform borderWidthLoc $= GL.Index1 (0 :: GLfloat)

      -- bind attributes
      bindGeometry shaderInfo labyrinthBuffersInfo

      -- bind indice
      bindBuffer ElementArrayBuffer $= Just (indice labyrinthBuffersInfo)

      drawElements GL.Lines (indiceCount labyrinthBuffersInfo) GL.UnsignedInt offset0

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

resize :: IORef Mat44f -> IORef GLfloat -> GLFW.WindowSizeCallback
resize projMatRef zoomRef size@(GL.Size w h) = do
  zoom <- get zoomRef
  GL.viewport $= (GL.Position 0 0, size)
  projMatRef $= (LAF.scale zoom $ LAF.orthoMatrixFromScreen w h 2)
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
  ki <- GLFW.getKey 'I'
  kpl <- GLFW.getKey GLFW.KP_ADD
  kmi <- GLFW.getKey GLFW.KP_SUBTRACT
  kpd <- GLFW.getKey GLFW.PAGEDOWN
  kpu <- GLFW.getKey GLFW.PAGEUP

  let nR = released kn n
  let lR = released kl l
  let sR = released ks s
  let dR = released kd d
  let dI = released ki i

  let newKeyState = keyState { n = kn
                             , s = ks
                             , l = kl
                             , d = kd
                             , i = ki
                             }

  let scaleF = if kpl == Press then 0.01 else if kmi == Press then (-0.01) else 0
  let dScale = max 0 $ min 1 $ scaleF + depthScale state

  let scaleExp = if kpu == Press then 1.01 else if kpd == Press then (1/1.01) else 1
  let expFact = max 1 $ scaleExp * explodedFactor state

  return state { keys = newKeyState
               , drawSolid = if sR then not $ drawSolid state else drawSolid state
               , drawNormals = if nR then not $ drawNormals state else drawNormals state
               , drawMazePath = if lR then not $ drawMazePath state else drawMazePath state
               , depthRender = if dR then not $ depthRender state else depthRender state
               , depthInvert = if dI then not $ depthInvert state else depthInvert state
               , depthScale = dScale
               , explodedFactor = expFact
               }

  where released newK oldK = newK == Release && newK /= oldK


updateZoom :: GLfloat -> GlobalState -> IO ()
updateZoom zoomFactor state = do
  if zoomFactor /= 1
    then do
      m <- get $ projMat state
      z <- get $ zoom state
      zoom state $= zoomFactor * z
      projMat state $= LAF.scale zoomFactor m
    else
      return ()


applyMouseActions :: GLfloat -> MouseState -> GlobalState -> IO GlobalState
applyMouseActions speed newMouseState global = do
  updateZoom zoomF global
  if leftButton newMouseState == Press
    then
      -- update model rotation matrix
      return global' { modelMat = LAF.naiveRotMat (diffX * speed) (diffY * speed) `LAF.multMat` modelMat global }
    else
      return global'
  where
    oldMouse = mouse global
    diffX = fromIntegral $ (mouseX newMouseState) - (mouseX oldMouse)
    diffY = fromIntegral $ (mouseY newMouseState) - (mouseY oldMouse)
    diffWheel = wheel newMouseState - wheel oldMouse
    zoomF = 1.01 ** (fromIntegral diffWheel)
    global' = global { mouse = newMouseState }


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
  newGlobal <- applyMouseActions speed newMouseState newGlobal0

  -- prepare matrices for rendering
  p <- get $ projMat newGlobal
  let vp = p `LAF.multMat` viewMat newGlobal
  let mvp = vp `LAF.multMat` modelMat newGlobal

  -- render
  render (simTime newGlobal)
         (drawSolid newGlobal)
         (drawNormals newGlobal)
         (drawMazePath newGlobal)
         (if depthRender newGlobal then if depthInvert newGlobal then -1 else 1 else 0)
         (depthScale newGlobal)
         (explodedFactor newGlobal)
         mvp
         (glids newGlobal)

  -- exit if window closed or Esc pressed
  esc <- GLFW.getKey GLFW.ESC
  q <- GLFW.getKey 'Q'
  open <- GLFW.getParam GLFW.Opened
  if open && esc /= GLFW.Press && q /= GLFW.Press
    then loop action' newGlobal
    else return newGlobal


intArgument :: String -> Int -> [String] -> Int
intArgument arg defaultValue args =
  -- cuts input
  let cutsStr = strArgument arg args in
  maybe defaultValue
        (\str -> case reads str of
          [] -> defaultValue
          [(i, _)] -> i)
        cutsStr

boolArgument :: String -> [String] -> Bool
boolArgument arg args = [] /= filter (== arg) args


strArgument :: String -> [String] -> Maybe String
strArgument arg args = listToMaybe $ drop 1 $ dropWhile (/= arg) args


loadModel :: GlobalState -> VC.VoronoiModel GLfloat -> Labyrinth Int -> IO GlobalState
loadModel global@GlobalState{..} vm laby = do

  t0 <- get time
  let GLIDs{..} = glids

  let faces = VC.faces vm
  let fc = VC.faceCount vm

  let (depths, maxDepth) = depthMap laby
  let (vertexBuffer, ids, centerBuffer, mazeBuffer, normalBuffer) = toBufferData faces depths maxDepth

  -- clean gl buffers and recreate them with proper data
  cleanBuffers objectBuffersInfo
  cleanBuffers normalsBuffersInfo

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


  -- computing the vertice of the maze path is complicated and heavy. dont do it unless specifically requested.
  newLabyrinthBuffersInfo <- if computeMazePath
    then do
      cleanBuffers labyrinthBuffersInfo

      t3 <- get time
      let (pathVertice, pathMazeData) = labyrinthToPathVertice faces laby maxDepth
      putStrLn $ show $ length pathVertice
      t4 <- get time
      putStrLn $ "path verts duration:\t" ++ (show $ t4 - t3)
      let pathIndice = labyrinthToPathIndice 0 laby
      putStrLn $ show $ length pathIndice
      t5 <- get time
      putStrLn $ "path ids duration:\t" ++ (show $ t5 - t4)

      newLabyrinthBuffersInfo <- loadBuffers (concatMap (\(G.Point3f x y z) -> [1.001*x,1.001*y,1.001*z]) pathVertice)
                                             pathIndice
                                             Nothing
                                             Nothing
                                             (Just pathMazeData)

      return newLabyrinthBuffersInfo
    else
      return labyrinthBuffersInfo

  t3 <- get time
  putStrLn $ "load model duration:\t" ++ show (t3 - t0)

  let newGlids = glids { objectBuffersInfo = newBuffersInfo
                       , normalsBuffersInfo = newNormalsBuffersInfo
                       , labyrinthBuffersInfo = newLabyrinthBuffersInfo
                       }

  return global { glids = newGlids }


main :: IO ()
main = do

  args <- getArgs

  -- fullscreen flag
  let fullScreenRequest = boolArgument "--f" args

  -- compute and render maze path
  let computeMazePath = boolArgument "--p" args

  -- seed input
  let seedStr = maybe "imullinati" id $ strArgument "--s" args
  let seed = seedForString seedStr

  putStrLn $ "seed:\t" ++ seedStr

  let cuts = intArgument "--c" 1000 args
  let mazeDepthGap = intArgument "--g" 28 args


  -- initialize early to have access to time
  GLFW.initialize

  let cuttableModel = VC.fromModel icosahedron

  t0 <- get time
  let (rndCuts, seed') = generateRndCuts cuts seed
  putStrLn $ "cuts:\t" ++ show cuts
  let rndCutsModel = foldr (\(t,p) m  -> VC.cutModelFromAngles t p m) cuttableModel $ reverse rndCuts
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

  t2 <- get time
  let (laby, seed'') = labyrinth1 seed' mazeDepthGap $ VC.faces rndCutsModel
  putStrLn $ "mazes sizes:\t" ++ show (size laby)
  t3 <- get time
  putStrLn $ "laby gen duration:\t" ++ show (t3 - t2)

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
  glstuff <- initGL

  -- init global state
  projMat <- newIORef identity
  zoom <- newIORef 1.75
  let state0 = GlobalState { viewMat = LAF.viewMatOf (pi/2) (pi/2) 1
                           , drawSolid = True
                           , drawNormals = False
                           , computeMazePath = computeMazePath
                           , drawMazePath = True
                           , mazeDepthGap = mazeDepthGap
                           , depthRender = True
                           , depthInvert = True
                           , depthScale = fromIntegral mazeDepthGap / 100
                           , explodedFactor = 1
                           , mouse = defaultMouseState
                           , glids = glstuff
                           , simTime = 0
                           , keys = defaultKeyState
                           , projMat = projMat
                           , modelMat = identity
                           , zoom = zoom
                           }

  state <- loadModel state0 rndCutsModel laby

  -- setup stuff
  GLFW.swapInterval       $= 1 -- vsync
  GLFW.windowTitle        $= "Voronyi maze"
  GLFW.windowSizeCallback $= resize projMat zoom
  -- main loop
  lastState <- loop waitForPress state
  -- exit
  cleanBuffers $ objectBuffersInfo $ glids lastState
  cleanBuffers $ normalsBuffersInfo $ glids lastState
  cleanBuffers $ labyrinthBuffersInfo $ glids lastState
  GLFW.closeWindow
  GLFW.terminate