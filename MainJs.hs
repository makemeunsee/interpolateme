import FlatModel ( FlatModel, fromModel, vertice, verticePerFace, normals, centers, indice, span, applyRndTranslationsToVertice )
import FloretSphere ( polyhedrons )
import GLGenericFunctions ( OrbitingState (OrbitingState), theta, phi, distance )
import qualified GLGenericFunctions as GF
import qualified Geometry as G
import qualified Data.Vec as V

import Haste
import Haste.Prim
import Haste.Foreign

--type Document = JSAny
--
--foreign import ccall "objWrite" write :: Document -> JSString -> IO ()
--foreign import ccall "getDocument" getDocument :: IO Document


models :: [FlatModel Float Int Int]
models = map fromModel polyhedrons

modelsSize :: IO Int
modelsSize = return $ length models

verticeOf :: Int -> IO [Float]
verticeOf i = return $ vertice $ models !! i

vpfOf :: Int -> IO [Int]
vpfOf i = return $ verticePerFace $ models !! i

normalsOf :: Int -> IO [Float]
normalsOf i = return $ normals $ models !! i

centersOf :: Int -> IO [Int]
centersOf i = return $ centers $ models !! i

indiceOf :: Int -> IO [Int]
indiceOf i = return $ indice $ models !! i

spanOf :: Int -> IO Float
spanOf i = return $ FlatModel.span $ models !! i

updateViewMat :: Float -> Float -> Float -> IO [Float]
updateViewMat t p d =
  return $ V.matToList $ GF.viewMatOf OrbitingState { theta = t, phi = GF.limitAngle p, distance = d }

orthoMatrixFromScreen :: Int -> Int -> IO [Float]
orthoMatrixFromScreen w h = return $ V.matToList $ G.orthoMatrixFromScreen w h

directionFromOrigin :: Float -> Float -> Float -> IO [Float]
directionFromOrigin theta phi dist = do
  let G.Point3f x y z = GF.orbitingEyeForModel V.identity $ OrbitingState { theta = theta, phi = phi, distance = dist }
  return [x, y, z]

normedDirectionToOrigin :: Float-> Float -> IO [Float]
normedDirectionToOrigin theta phi = return $ [x, y, z]
  where G.Point3f x y z = GF.orbitCenterDirection OrbitingState { theta = theta, phi = phi, distance = 1 }

rndAlongAxis :: [Float] -> Float -> Float -> Float -> [Float] -> [Int] -> IO [Float]
rndAlongAxis a b c d e f = return $ applyRndTranslationsToVertice a b c d e f

main = do
  export (toJSStr "modelsLength") modelsSize
  export (toJSStr "verticeOf") verticeOf
  export (toJSStr "verticeCountPerFaceOf") vpfOf
  export (toJSStr "normalsOf") normalsOf
  export (toJSStr "centersOf") centersOf
  export (toJSStr "indiceOf") indiceOf
  export (toJSStr "spanOf") spanOf
  export (toJSStr "updateViewMat") updateViewMat
  export (toJSStr "orthoMatrixFromScreen") orthoMatrixFromScreen
  export (toJSStr "normedDirectionToOrigin") normedDirectionToOrigin
  export (toJSStr "directionFromOrigin") directionFromOrigin
  export (toJSStr "rndFacesAlongAxis") rndAlongAxis