{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (
    main
) where

import           System.Environment            (getArgs)

-- import           Data.Colour.RGBSpace.HSL      (hsl)
-- import           Data.Colour.RGBSpace          (channelRed, channelGreen, channelBlue)
import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL     (($=))
import           Graphics.Rendering.OpenGL.GL  (AttribLocation (AttribLocation),
                                                BufferObject, BufferTarget (ElementArrayBuffer),
                                                GLfloat, GLint, GLuint, Program, UniformLocation (UniformLocation),
                                                attachShader, attribLocation,
                                                bindBuffer, createProgram,
                                                currentProgram, detachShader,
                                                drawElements, get, linkProgram,
                                                linkStatus, programInfoLog,
                                                releaseShaderCompiler, uniform,
                                                uniformLocation,
                                                validateProgram, validateStatus)
import qualified Graphics.Rendering.OpenGL.GL  as GL
import           Graphics.Rendering.OpenGL.Raw (glBindBuffer, glBufferData,
                                                glDeleteBuffers,
                                                glDisableVertexAttribArray,
                                                glEnableVertexAttribArray,
                                                glGenBuffers,
                                                glGetBufferSubData,
                                                glUniform4fv,
                                                glUniformMatrix4fv,
                                                glVertexAttribPointer,
                                                gl_ARRAY_BUFFER, gl_FLOAT,
                                                gl_STATIC_DRAW)
import           Graphics.UI.GLFW              as GLFW

import           Control.Monad                 (when)

import           Foreign                       (Ptr, alloca, castPtr, free,
                                                fromBool, mallocArray, nullPtr,
                                                peek, peekArray, sizeOf, with,
                                                withArrayLen)

import           Data.Foldable                 (foldr', foldrM, toList)
import           Data.IORef                    (IORef, newIORef)
import qualified Data.Map                      as Map
import           Data.Maybe                    (listToMaybe, fromMaybe)
import qualified Data.Sequence                 as Seq
import           Data.Vec                      (Mat44, Vec4, identity)
import           Random.MWC.Pure               (Seed, range_random)

import qualified Geometry                      as G
import           Labyrinth
import qualified LinAlgFunctions               as LAF
import           ListUtil
import           Models
import           RandomUtil
import qualified VoronoiCut                    as VC
import           CubeHelix


type Vec4f = Vec4 GLfloat
type Mat44f = Mat44 GLfloat


data Action = Action (IO Action, MouseState -> MouseState)


data KeyState = KeyState { n     :: KeyButtonState
                         , s     :: KeyButtonState
                         , l     :: KeyButtonState
                         , d     :: KeyButtonState
                         , i     :: KeyButtonState
                         , t     :: KeyButtonState
                         , space :: KeyButtonState
                         }


data MouseState = MouseState { mouseX     :: GLint
                             , mouseY     :: GLint
                             , wheel      :: Int
                             , leftButton :: KeyButtonState
                             }
                  deriving Show


data GlobalState = GlobalState { viewMat          :: Mat44f
                               , drawSolid        :: Bool
                               , drawNormals      :: Bool
                               , drawMazePath     :: Bool
                               , thickPath        :: Bool
                               , depthRender      :: Bool
                               , depthInvert      :: Bool
                               , depthScale       :: GLfloat
                               , explodedFactor   :: GLfloat
                               , mouse            :: MouseState
                               , modelMat         :: Mat44f
                               , glids            :: GLIDs
                               , simTime          :: GLfloat
                               , keys             :: KeyState
                               , projMat          :: IORef Mat44f
                               , zoom             :: IORef GLfloat
                               , faces            :: Seq.Seq (VC.Face GLfloat)
                               , maze             :: Labyrinth Int
                               , depths           :: Map.Map Int [Int]
                               , depthMin         :: Int
                               , depthMax         :: Int
                               , inMazeNeighbours :: Map.Map (Int, Int) [(Int, Int)]
                               , seed             :: Seed
                               , highlight        :: Bool
                               }


-- state inits


defaultKeyState :: KeyState
defaultKeyState = KeyState Release Release Release Release Release Release Release


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
data ShaderInfo = ShaderInfo { prog            :: Program
                             , vertexAttrib    :: GLuint
                             , normalAttrib    :: GLuint
                             , centerAttrib    :: GLuint
                             , mazeAttrib      :: GLuint
                             , faceIdAttrib    :: GLuint
                             , deepColorAttrib :: GLuint
                             }


data BuffersInfo = BuffersInfo { indice              :: BufferObject
                               , indiceCount         :: GLint
                               , vertexBuffersLength :: Int
                               , vertexBufferId      :: GLuint
                               , ext                 :: OptionGeomInfo
                               }


data OptionGeomInfo = OptionGeomInfo { normalBufferId    :: Maybe GLuint
                                     , centerBufferId    :: Maybe GLuint
                                     , mazeBufferId      :: Maybe GLuint
                                     , faceIdBufferId    :: Maybe GLuint
                                     , deepColorBufferId :: Maybe GLuint
                                     }


data GLIDs = GLIDs { shaderInfo           :: ShaderInfo
                   , objectBuffersInfo    :: BuffersInfo
                   , labyrinthBuffersInfo :: BuffersInfo
                   , normalsBuffersInfo   :: BuffersInfo
                   }


createShader :: FilePath -> FilePath -> IO ShaderInfo
createShader vertexShaderFile fragShaderFile = do
  prog <- loadProgram vertexShaderFile fragShaderFile
  AttribLocation vertexAttrib <- get (attribLocation prog "position")
  AttribLocation normalAttrib <- get (attribLocation prog "normal")
  AttribLocation centerAttrib <- get (attribLocation prog "a_centerFlag")
  AttribLocation mazeAttrib <- get (attribLocation prog "a_mazeDepth")
  AttribLocation faceIdAttrib <- get (attribLocation prog "a_faceId")
  AttribLocation deepColorAttrib <- get (attribLocation prog "a_depthModeColor")
  return ShaderInfo{..}


loadOptionalBuffers :: Maybe [GLfloat] -> Maybe [GLfloat] -> Maybe [GLfloat] -> Maybe [GLfloat] -> Maybe [GLfloat] -> IO OptionGeomInfo
loadOptionalBuffers m_normalData m_centersData m_mazeData m_faceIdData m_deepColorData = do
  normalBufferId <- loadOptionalBuffer m_normalData
  centerBufferId <- loadOptionalBuffer m_centersData
  mazeBufferId <- loadOptionalBuffer m_mazeData
  faceIdBufferId <- loadOptionalBuffer m_faceIdData
  deepColorBufferId <- loadOptionalBuffer m_deepColorData
  return OptionGeomInfo{..}


loadOptionalBuffer :: Maybe [GLfloat] -> IO (Maybe GLuint)
loadOptionalBuffer m_data = case m_data of
                              Nothing -> return Nothing
                              Just dat -> do id <- fillNewFloatBuffer dat
                                             return $ Just id


loadBuffers :: [GLfloat] -> [GLuint] -> Maybe [GLfloat] -> Maybe [GLfloat] -> Maybe [GLfloat] -> Maybe [GLfloat] -> Maybe [GLfloat] -> IO BuffersInfo
loadBuffers verticeData indiceData m_normalData m_centersData m_mazeData m_faceIdData m_deepColorData = do

  indice <- makeBuffer ElementArrayBuffer indiceData
  let indiceCount = indexCount indiceData
  let vertexBuffersLength = length verticeData
  vertexBufferId <- fillNewFloatBuffer verticeData
  ext <- loadOptionalBuffers m_normalData m_centersData m_mazeData m_faceIdData m_deepColorData
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
  objectBuffersInfo <- loadBuffers [] [] Nothing Nothing Nothing Nothing Nothing
  normalsBuffersInfo <- loadBuffers [] [] Nothing Nothing Nothing Nothing Nothing
  labyrinthBuffersInfo <- loadBuffers [] [] Nothing Nothing Nothing Nothing Nothing

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
  cleanOptionalBuffer faceIdBufferId
  cleanOptionalBuffer deepColorBufferId


cleanOptionalBuffer :: Maybe GLuint -> IO ()
cleanOptionalBuffer m_id =  case m_id of Nothing -> return ()
                                         Just i  -> with i $ glDeleteBuffers 1


-- rendering code


bindGeometry :: ShaderInfo -> BuffersInfo -> IO ()
bindGeometry si@ShaderInfo{..} BuffersInfo{..} = do
  bindFloatBufferToAttrib 3 vertexBufferId vertexAttrib
  bindOptionalGeometries ext si


bindOptionalGeometries :: OptionGeomInfo -> ShaderInfo -> IO ()
bindOptionalGeometries OptionGeomInfo{..} ShaderInfo{..} = do
  bindOptionalGeometry 3 normalAttrib normalBufferId
  bindOptionalGeometry 3 deepColorAttrib deepColorBufferId
  bindOptionalGeometry 1 centerAttrib centerBufferId
  bindOptionalGeometry 1 mazeAttrib mazeBufferId
  bindOptionalGeometry 1 faceIdAttrib faceIdBufferId


bindOptionalGeometry :: GLint -> GLuint -> Maybe GLuint -> IO ()
bindOptionalGeometry size attribId m_id =
  case m_id of Nothing -> return ()
               Just i  -> bindFloatBufferToAttrib size i attribId



unbindGeometry :: ShaderInfo -> IO ()
unbindGeometry ShaderInfo{..} = do
  glDisableVertexAttribArray vertexAttrib
  glDisableVertexAttribArray normalAttrib
  glDisableVertexAttribArray centerAttrib
  glDisableVertexAttribArray mazeAttrib
  glDisableVertexAttribArray faceIdAttrib
  glDisableVertexAttribArray deepColorAttrib


bindUniformMatrix :: Program -> String -> Mat44f -> IO ()
bindUniformMatrix prog uName mat = do
  (UniformLocation matLoc) <- get $ uniformLocation prog uName
  with mat $ glUniformMatrix4fv matLoc 1 (fromBool True) . castPtr


bindUniformVector :: Program -> String -> Vec4f -> IO ()
bindUniformVector prog uName vec = do
  (UniformLocation vLoc) <- get $ uniformLocation prog uName
  with vec $ glUniform4fv vLoc 1 . castPtr


render :: GLfloat -> Bool -> Bool -> Bool -> Bool -> GLfloat -> GLfloat -> GLfloat -> Mat44f -> GLIDs -> Int -> GLfloat -> IO ()
render t drawSolid drawNormals drawMazePath thickPath depthMode depthScale explodedFactor mvp GLIDs{..} litFaceId litFaceDepth = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  let ShaderInfo{..} = shaderInfo

  currentProgram $= Just prog

  -- which cell should be lit
  litFaceLoc <- get $ uniformLocation prog "u_litFace"
  uniform litFaceLoc $= GL.Index1 (fromIntegral litFaceId :: GLfloat)
  litDepthLoc <- get $ uniformLocation prog "u_litDepth"
  uniform litDepthLoc $= GL.Index1 litFaceDepth

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

  when drawNormals $
    do uniform colLoc $= GL.Color3 0 1 (0 :: GLfloat)
       uniform bColLoc $= GL.Color3 0 1 (0 :: GLfloat)
       uniform borderWidthLoc $= GL.Index1 (0 :: GLfloat)
       -- bind attributes
       bindGeometry shaderInfo normalsBuffersInfo
       -- bind indice
       bindBuffer ElementArrayBuffer $= Just (indice normalsBuffersInfo)
       drawElements GL.Lines (indiceCount normalsBuffersInfo) GL.UnsignedInt offset0
       unbindGeometry shaderInfo


  let drawSolidFct = do
                       -- bind attributes
                       bindGeometry shaderInfo objectBuffersInfo
                       -- bind indice
                       bindBuffer ElementArrayBuffer $= Just (indice objectBuffersInfo)
                       drawElements GL.Triangles (indiceCount objectBuffersInfo) GL.UnsignedInt offset0
                       unbindGeometry shaderInfo


  -- draw object
  if drawSolid
    then do
      uniform colLoc $= GL.Color3 1 1 (1 :: GLfloat)
      uniform bColLoc $= GL.Color3 0.05 0.05 (0.05 :: GLfloat)
      uniform borderWidthLoc $= GL.Index1 (1.0 :: GLfloat)
      drawSolidFct
    else when (depthMode == 0) $ -- no depth mode, no solid -> draw solid black
      do uniform colLoc $= GL.Color3 0 0 (0 :: GLfloat)
         uniform bColLoc $= GL.Color3 0 0 (0 :: GLfloat)
         uniform borderWidthLoc $= GL.Index1 (0 :: GLfloat)
         drawSolidFct

  -- draw labyrinth path
  when drawMazePath $
    do uniform colLoc $= GL.Color3 1 0.1 (0 :: GLfloat)
       uniform bColLoc $= GL.Color3 1 0.1 (0 :: GLfloat)
       uniform borderWidthLoc $= GL.Index1 (0.0 :: GLfloat)
       -- bind attributes
       bindGeometry shaderInfo labyrinthBuffersInfo
       -- bind indice
       bindBuffer ElementArrayBuffer $= Just (indice labyrinthBuffersInfo)
       if thickPath
         then drawElements GL.Triangles (indiceCount labyrinthBuffersInfo) GL.UnsignedInt offset0
         else drawElements GL.Lines (indiceCount labyrinthBuffersInfo) GL.UnsignedInt offset0
       unbindGeometry shaderInfo

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
  projMatRef $= LAF.scale zoom (LAF.orthoMatrixFromScreen w h 2)
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
    GLFW.Press   ->
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
  let GlobalState{..} = state
  let keyState@KeyState{..} = keys

  -- read key inputs
  sp <- GLFW.getKey ' '
  kn <- GLFW.getKey 'N'
  ks <- GLFW.getKey 'S'
  kl <- GLFW.getKey 'L'
  kd <- GLFW.getKey 'D'
  ki <- GLFW.getKey 'I'
  kt <- GLFW.getKey 'T'
  kpl <- GLFW.getKey GLFW.KP_ADD
  kmi <- GLFW.getKey GLFW.KP_SUBTRACT
  kpd <- GLFW.getKey GLFW.PAGEDOWN
  kpu <- GLFW.getKey GLFW.PAGEUP

  let nR = released kn n
  let lR = released kl l
  let sR = released ks s
  let dR = released kd d
  let iR = released ki i
  let tR = released kt t
  let spR = released sp space

  let newKeyState = keyState { n = kn
                             , s = ks
                             , l = kl
                             , d = kd
                             , i = ki
                             , t = kt
                             , space = sp
                             }

  let scaleF | kpl == Press = 0.01
             | kmi == Press = -0.01
             | otherwise    = 0
  let dScale = max 0 $ min 1 $ scaleF + depthScale

  let scaleExp | kpu == Press = 1.01
               | kpd == Press = (1/1.01)
               | otherwise    = 1
  let expFact = max 1 $ scaleExp * explodedFactor

  let nState = state { keys = newKeyState
                     , drawSolid = if sR then not drawSolid else drawSolid
                     , drawNormals = if nR then not drawNormals else drawNormals
                     , drawMazePath = if lR then not drawMazePath else drawMazePath
                     , thickPath = if tR then not thickPath else thickPath
                     , depthRender = if dR then not depthRender else depthRender
                     , depthInvert = if iR then not depthInvert else depthInvert
                     , highlight = if spR then not highlight else highlight
                     , depthScale = dScale
                     , explodedFactor = expFact
                     }

  if tR
    then do
      newLabyrinthBuffersInfo <- loadMazePath nState
      let newGlids = glids { labyrinthBuffersInfo = newLabyrinthBuffersInfo }
      return nState { glids = newGlids }
    else
      return nState



  where released newK oldK = newK == Release && newK /= oldK


updateZoom :: GLfloat -> GlobalState -> IO ()
updateZoom zoomFactor state =
  when (zoomFactor /= 1) $
    do m <- get $ projMat state
       z <- get $ zoom state
       zoom state $= zoomFactor * z
       projMat state $= LAF.scale zoomFactor m


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
    diffX = fromIntegral $ mouseX newMouseState - mouseX oldMouse
    diffY = fromIntegral $ mouseY newMouseState - mouseY oldMouse
    diffWheel = wheel newMouseState - wheel oldMouse
    zoomF = 1.05 ** fromIntegral diffWheel
    global' = global { mouse = newMouseState }


xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a


nextPosition :: Seed -> MazePosition -> Seq.Seq (VC.Face a) -> Map.Map Int [Int] -> (MazePosition, Seed)
nextPosition seed MazePosition{..} faces depths =
  if null okNs then
    (MazePosition faceId depth ((-1)*direction), seed)
  else
    (MazePosition newFaceId newDepth direction, seed')
  where
    neighbours = VC.neighbours $ Seq.index faces faceId
    fDepths faceId = fromMaybe [] (Map.lookup faceId depths)
    nsWithDepth = concatMap (\fId -> zip (repeat fId) $ fDepths fId) neighbours
    okNs = filter (\(_, d) -> ((d < depth) `xor` (direction > 0)) && abs (depth - d) == 1) nsWithDepth
    l = length okNs
    (rndIndex, seed') = range_random (0, l) seed
    (newFaceId, newDepth) = okNs !! rndIndex


data MazePosition = MazePosition { faceId :: Int, depth :: Int, direction :: Int }
                    deriving Show


loop :: IO Action -> GlobalState -> (MazePosition, Double) -> IO GlobalState
loop action global (litPosition, lastPositionChange) = do

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
  let GlobalState{..} = newGlobal

  -- prepare matrices for rendering
  p <- get projMat
  let vp = p `LAF.multMat` viewMat
  let mvp = vp `LAF.multMat` modelMat

  -- render
  render simTime
         drawSolid
         drawNormals
         drawMazePath
         thickPath
         (if depthRender then if depthInvert then -1 else 1 else 0)
         depthScale
         explodedFactor
         mvp
         glids
         (if highlight then faceId litPosition else -1)
         (fromIntegral (depth litPosition + depthMin) / fromIntegral (depthMax - depthMin))

  t <- get time
  let (litUpdate, seed') = if highlight && lastPositionChange + 0.05 <= t then
                             let (updatedPosition, newSeed) = nextPosition seed
                                                                           litPosition
                                                                           faces
                                                                           depths in
                             ((updatedPosition, t), newSeed)
                           else
                             ((litPosition, lastPositionChange), seed)

  -- exit if window closed or Esc pressed
  esc <- GLFW.getKey GLFW.ESC
  q <- GLFW.getKey 'Q'
  open <- GLFW.getParam GLFW.Opened
  if open && esc /= GLFW.Press && q /= GLFW.Press
    then loop action' newGlobal { seed = seed' } litUpdate
    else return newGlobal { seed = seed' }


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


loadModel :: GlobalState -> IO GlobalState
loadModel global@GlobalState{..} = do

  t0 <- get time
  let GLIDs{..} = glids

  let (vertexBuffer, ids, centerBuffer, mazeBuffer, normalBuffer, faceIdsBuffer) = toBufferData faces depths depthMin depthMax

  -- default cubehelix palette
  let colorFct = cubeHelixInterpolationFct 1 (-240) 0.5 1 300 0.5 0
  -- Matteo Niccoli’s perceptual rainbow
  let colorFct' = cubeHelixInterpolationFct 1 300 (5/12) 0.1 60 (25/24) 0.9

  let deepColorsBuffer = concatMap (\d -> colorFct' d) mazeBuffer

--  -- cubehelix rainbow as seen here: http://bl.ocks.org/mbostock/310c99e53880faec2434
--  let colorFct0 = cubeHelixInterpolationFct 1 (-100) 0.75 0.35 80 1.5 0.8
--  let colorFct1 = cubeHelixInterpolationFct 1 80 1.5 0.8 260 0.75 0.3
--  let deepColorsBuffer = concatMap (\d ->
--                                     if d < 0.5 then colorFct0 $ d*2
--                                                else colorFct1 $ (d-0.5)*2
--                                   )
--                                   mazeBuffer

  -- clean gl buffers and recreate them with proper data
  cleanBuffers objectBuffersInfo
  cleanBuffers normalsBuffersInfo

  newBuffersInfo <- loadBuffers (concatMap G.pointToArr vertexBuffer)
                                ids
                                (Just $ concatMap G.pointToArr normalBuffer)
                                (Just centerBuffer)
                                (Just mazeBuffer)
                                (Just faceIdsBuffer)
                                (Just deepColorsBuffer)

  let faceCenters = map VC.seed $ toList faces -- on the unit sphere so they're normalized too
  let verticeOfNormalsBuffer = concatMap (\(G.Point3f cx cy cz) ->
                                         [cx, cy, cz, cx + 0.1 * cx, cy + 0.1 * cy, cz + 0.1 * cz])
                                         faceCenters
  newNormalsBuffersInfo <- loadBuffers verticeOfNormalsBuffer
                                       (take (length verticeOfNormalsBuffer) [0..])
                                       Nothing
                                       Nothing
                                       Nothing
                                       Nothing
                                       Nothing



  t1 <- get time
  putStrLn $ "load model duration:\t" ++ show (t1 - t0)

  newLabyrinthBuffersInfo <- loadMazePath global

  let newGlids = glids { objectBuffersInfo = newBuffersInfo
                       , normalsBuffersInfo = newNormalsBuffersInfo
                       , labyrinthBuffersInfo = newLabyrinthBuffersInfo
                       }

  return global { glids = newGlids }


loadMazePath :: GlobalState -> IO BuffersInfo
loadMazePath GlobalState{..} = do

  cleanBuffers $ labyrinthBuffersInfo glids

  t0 <- get time
  if thickPath
    then do
      let (pathVs, pathIds, pathCs, pathNs, pathDs) = toThickPathBufferData faces depthMin depthMax inMazeNeighbours
      putStrLn $ "path size:\t" ++ show (length pathVs)
      t1 <- get time
      putStrLn $ "path buffers duration:\t" ++ show (t1 - t0)
      loadBuffers (concatMap (\(G.Point3f x y z) -> [1.001*x,1.001*y,1.001*z]) pathVs)
                  pathIds
                  (Just $ concatMap G.pointToArr pathNs)
                  (Just pathCs)
                  (Just pathDs)
                  Nothing
                  Nothing
    else do
      let (pathVs, pathIds, pathNs, pathDs) = toPathBufferData faces depthMin depthMax inMazeNeighbours
      putStrLn $ "path size:\t" ++ show (length pathVs)
      t1 <- get time
      putStrLn $ "path buffers duration:\t" ++ show (t1 - t0)
      loadBuffers (concatMap (\(G.Point3f x y z) -> [1.001*x,1.001*y,1.001*z]) pathVs)
                  pathIds
                  (Just $ concatMap G.pointToArr pathNs)
                  Nothing
                  (Just pathDs)
                  Nothing
                  Nothing


uniformToSphericCoordinates :: RealFloat a => (a, a) -> (a, a)
uniformToSphericCoordinates (u,v) = (2*pi*u, acos $ 2*v - 1)


uniformModel :: RealFloat a => Int -> VC.VoronoiModel a
uniformModel bn = VC.VoronoiModel rows
  where
    n = max 3 bn
    n' = ceiling (fromIntegral n / 2)
    southFaceId = 0
    northFaceId = 1 + n*(n'-2)
    k = 1 / fromIntegral n
    k' = 1 / fromIntegral n'
    upoints = [map ((\(t,p) -> LAF.latLongPosition t p 1) . (\(u,v) -> (2*pi*u, pi - v*pi))) [(fromIntegral u * k, fromIntegral v * k') | u <- [0..n-1]] |  v <- [1..n'-1]]

    bottomFace = VC.Face (G.Point3f 0 (-1) 0) (head upoints) (take n [1..])
    rows = rowsGen upoints 1 (Seq.singleton bottomFace)

    rowsGen (r0:r1:rs) o acc = rowsGen (r1:rs) (o+n) (acc Seq.>< row r0 r1 o)
    rowsGen [r] o acc = rowsGen [] (o+n) (acc Seq.|> topFace r o)
    rowsGen [] _ acc = acc
    row r0 r1 o = fst $ foldr (\((p0,p1),(p2,p3)) (acc, o') -> (acc Seq.|> newFace p0 p1 p2 p3 o', o'+1)) (Seq.empty, o) $ cyclicConsecutivePairs $ zip r0 r1

    topFace r o = VC.Face (G.Point3f 0 1 0) (reverse r) (take n $ map (o-) [1..])

    newFace p0 p1 p2 p3 o = VC.Face (G.normalized $ G.barycenter [p0,p1,p2,p3])
                                    [p0,p1,p3,p2]
                                    [ min (o+n) northFaceId
                                    , max (o-n) southFaceId
                                    , if (o-1) `mod` n == 0 then o + n - 1 else o-1
                                    , if o `mod` n == 0 then o + 1 - n else o+1 ]


twoPyramids :: RealFloat a => Int -> VC.VoronoiModel a
twoPyramids bn = VC.VoronoiModel faces
  where
    n = max 3 bn
    southP = G.Point3f 0 (-1) 0
    northP = G.Point3f 0 1 0
    k = 1 / fromIntegral n
    points = map ((\(t,p) -> LAF.latLongPosition t p 1) . uniformToSphericCoordinates) [(fromIntegral u * k, 0.5) | u <- [0..n-1]]
    faces = fst $ foldr (\(p0,p1) (acc, i) -> (acc Seq.|> newFace0 p0 p1 i Seq.|> newFace1 p0 p1 i, i+1)) (Seq.empty, 0) $ cyclicConsecutivePairs points
    newFace0 p0 p1 i = VC.Face (G.normalized $ G.barycenter [p0,p1,northP]) [p1,p0,northP] $ map within [2*i+1, 2*(i-1), 2*(i+1)]
    newFace1 p0 p1 i = VC.Face (G.normalized $ G.barycenter [p0,p1,southP]) [p1,southP,p0] $ map within [2*i, 2*i-1, 2*i+3]
    within i = (if i < 0 then i + 2*n else i) `mod` (2*n)


voronoiModel seed cuts = do
  t0 <- get time
  let (rndCuts, seed') = generatePairs cuts seed
  putStrLn $ "cuts:\t" ++ show (length rndCuts)
  when (cuts > 0) $
    do let lastCut = last rndCuts
       putStrLn $ "last cut:\t" ++ show lastCut
       let (t,p) = uniformToSphericCoordinates lastCut
       putStrLn $ "last cut seed:\t" ++ show (LAF.latLongPosition t p 1)

  -- apply the cuts to a base model, to obtain a random tessellation
  let cuttableModel = VC.fromModel icosahedron
  let rndCutsModel = foldr' (\(t,p) m -> VC.cutModelFromAngles t p m) cuttableModel $ map uniformToSphericCoordinates rndCuts
  putStrLn $ "first face seed:\t" ++ show (VC.seed $ Seq.index (VC.faces rndCutsModel) 0)
--  putStrLn "cut\tfaces\tduration"
--  (rndCutsModel, _) <- foldrM (\(t,p) (m,i)  -> do
--                                putStr $ show i
--                                t0 <- get time
--                                let m' = VC.cutModelFromAngles t p m
--                                putStr $ "\t" ++ (show $ VC.faceCount m') ++ "\t"
--                                t1 <- get time
--                                putStrLn $ show $ t1-t0
--                                return (m', i+1)
--                              )
--                              (cuttableModel, 0)
--                              rndCuts

  -- rough perf measurement
  t1 <- get time
--  putStrLn $ "topology:\t" ++ (show $ map VC.neighbours $ VC.faceList rndCutsModel)
  let topoComplexity = foldr' (\f acc -> acc + length (VC.neighbours f)) 0 $ VC.faces rndCutsModel
  putStrLn $ "topology complexity:\t" ++ show topoComplexity
  putStrLn $ "Truncation duration: " ++ show (t1 - t0)
  return (rndCutsModel, seed')


main :: IO ()
main = do


  args <- getArgs

  -- fullscreen flag
  let fullScreenRequest = boolArgument "--f" args

  -- seed input
  let seedStr = fromMaybe "sexyseed" $ strArgument "--s" args
  let seed = seedForString seedStr

  putStrLn $ "seed:\t" ++ seedStr

  let cuts = max 0 $ intArgument "--c" 8000 args
  let branchMax = max 1 $ intArgument "--m" cuts args
  let gapMin = max 1 $ intArgument "--g" branchMax args
  let rndDepth = boolArgument "--r" args
  let uniform = boolArgument "--u" args

  putStrLn $ "branch lmax:\t" ++ show branchMax
  putStrLn $ "gap frac:\t" ++ show gapMin
  putStrLn $ "rnd depth:\t" ++ show rndDepth
  putStrLn $ "uniform model:\t" ++ show uniform

  -- initialize early to have access to time
  GLFW.initialize

  --  let model = twoPyramids cuts
  (model, seed') <- if uniform
                      then
                        return (uniformModel $ ceiling $ sqrt $ fromIntegral cuts, seed)
                      else
                        voronoiModel seed cuts
--  let model = uniformModel cuts
--  let seed' = seed
--  (model,seed') <- voronoiModel seed cuts
--  putStrLn $ show model

  let faces = VC.faces model
  t2 <- get time

  -- create a maze from the tessellation faces
  let (laby, seed'') = depthFirstMaze seed' branchMax gapMin (not rndDepth) faces
--  let (laby, seed'') = breadthFirstMaze seed' faces
--  putStrLn $ "maze:\t" ++ show laby

  let cellCount = size laby
  putStrLn $ "maze size:\t" ++ show cellCount
  t3 <- get time
  putStrLn $ "laby gen duration:\t" ++ show (t3 - t2)

  -- extract maze depth data
  let (depths, depthMax, depthMin) = depthMap laby
  let depthSpan = depthMax - depthMin
  putStrLn $ "depth span:\t" ++ show depthSpan
  t4 <- get time
  putStrLn $ "laby depth mapping:\t" ++ show (t4 - t3)
  -- parent and children of each face
  let inMazeNeighbours = neighboursMap Nothing laby

  -- which cell to highlight first
  let findFaceInMaze s = let (firstFaceId, s') = range_random (0, Seq.length faces) s in
                         let m_firstDepths = Map.lookup firstFaceId depths in
                         maybe (findFaceInMaze s') (\ds -> (firstFaceId, ds, s')) m_firstDepths

  let (firstFaceId, firstDepths, seed''') = findFaceInMaze seed''
  let (dId, seed'''') = range_random (0, length firstDepths) seed'''
  let firstDepth = firstDepths !! dId
  putStrLn $ "first position:\t" ++ show (firstFaceId, firstDepth)

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
                           , drawMazePath = False
                           , thickPath = False
                           , depthRender = True
                           , depthInvert = True
                           , depthScale = 0.5
                           , explodedFactor = 1
                           , mouse = defaultMouseState
                           , glids = glstuff
                           , simTime = 0
                           , keys = defaultKeyState
                           , projMat = projMat
                           , modelMat = identity
                           , zoom = zoom
                           , faces = faces
                           , maze = laby
                           , depths = depths
                           , depthMin = depthMin
                           , depthMax = depthMax
                           , inMazeNeighbours = inMazeNeighbours
                           , seed = seed''''
                           , highlight = True
                           }

  state <- loadModel state0

  -- setup stuff
  GLFW.swapInterval       $= 1 -- vsync
  GLFW.windowTitle        $= "Voronyi maze"
  GLFW.windowSizeCallback $= resize projMat zoom
  -- main loop
  lastState <- loop waitForPress state (MazePosition firstFaceId firstDepth 1, 0)
  -- exit
  cleanBuffers $ objectBuffersInfo $ glids lastState
  cleanBuffers $ normalsBuffersInfo $ glids lastState
  cleanBuffers $ labyrinthBuffersInfo $ glids lastState
  GLFW.closeWindow
  GLFW.terminate
