{-# LANGUAGE RecordWildCards #-}

import FlatModel ( FlatModel (..), fromModel )
import FloretSphere ( polyhedrons )
import qualified Voronyi as Vor
import GLGenericFunctions ( OrbitingState (OrbitingState), theta, phi, distance )
import qualified GLGenericFunctions as GF
import qualified Geometry as G
import qualified Data.Vec as V

import Haste
import Haste.Prim
import Haste.Foreign


models :: [Opaque (Vor.VoronoiModel Float)]
models = map (toOpaque . Vor.toVoronoiModel) polyhedrons

loadModel :: Int -> IO (Opaque (Vor.VoronoiModel Float))
loadModel modelId = return $ models !! modelId

truncateModel :: Opaque (Vor.VoronoiModel Float) -> Float -> Float -> IO (Opaque (Vor.VoronoiModel Float))
truncateModel model theta phi = return $ toOpaque $ Vor.truncate 0.00001 cutNormal $ fromOpaque model
  where
    cutNormal = GF.latLongPosition GF.OrbitingState { theta = theta, phi = phi, distance = 1 }

toFlatModel :: Opaque (Vor.VoronoiModel Float) -> IO ([Float], [Float], [Float], [Int], [Int], Float)
toFlatModel model = return (vs, ns, cs, ids, vpf, span)
  where
   FlatModel vs ns cs ids vpf span = fromModel $ G.Model vertice polygons seeds
   Vor.VoronoiModel seeds vertice polygons = fromOpaque model

modelsSize :: IO Int
modelsSize = return $ length models

updateViewMat :: Float -> Float -> Float -> IO [Float]
updateViewMat t p d =
  return $ V.matToList $ GF.viewMatOf OrbitingState { theta = t, phi = p, distance = d }

orthoMatrixFromScreen :: Int -> Int -> IO [Float]
orthoMatrixFromScreen w h = return $ V.matToList $ G.orthoMatrixFromScreen w h

directionFromOrigin :: Float -> Float -> Float -> IO [Float]
directionFromOrigin theta phi dist = do
  let G.Point3f x y z = GF.orbitingEyeForModel V.identity $ OrbitingState { theta = theta, phi = phi, distance = dist }
  return [x, y, z]

normedDirectionToOrigin :: Float -> Float -> IO [Float]
normedDirectionToOrigin theta phi = return $ [x, y, z]
  where G.Point3f x y z = GF.orbitCenterDirection OrbitingState { theta = theta, phi = phi, distance = 1 }

latLongRotMat :: Float -> Float -> Float -> IO [Float]
latLongRotMat t p d =
  return $ V.matToList $ GF.latLongRotMat OrbitingState { theta = t, phi = p, distance = d }

main = do
  export (toJSStr "modelsLength") modelsSize
  export (toJSStr "loadModel") loadModel
  export (toJSStr "updateViewMat") updateViewMat
  export (toJSStr "orthoMatrixFromScreen") orthoMatrixFromScreen
  export (toJSStr "normedDirectionToOrigin") normedDirectionToOrigin
  export (toJSStr "directionFromOrigin") directionFromOrigin
  export (toJSStr "latLongRotMat") latLongRotMat
  export (toJSStr "truncate") truncateModel
  export (toJSStr "toFlatModel") toFlatModel
