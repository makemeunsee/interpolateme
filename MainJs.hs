{-# LANGUAGE RecordWildCards #-}

import FlatModel ( FlatModel (..), fromModel )
import FloretSphere ( polyhedrons )
import qualified PlaneCut as PC
import qualified GLGenericFunctions as GF
import qualified Geometry as G
import qualified Data.Vec as V

import Haste
import Haste.Prim
import Haste.Foreign


models :: [Opaque (PC.FacedModel Float)]
models = map (toOpaque . PC.fromModel) polyhedrons

loadModel :: Int -> IO (Opaque (PC.FacedModel Float))
loadModel modelId = return $ models !! modelId

truncateRotatedModel :: Opaque (PC.FacedModel Float) -> [Float] -> IO (Opaque (PC.FacedModel Float))
truncateRotatedModel model scaledModelMat = return $ toOpaque m'
  where
    m' = PC.cutModel 0.00001 plane $ fromOpaque model
    seed = G.vec4ToPoint3f $ G.multInvMatV (V.matFromList scaledModelMat) $ G.vec4 0 0 1
    -- normal of the plane
    G.Point3f kx ky kz = G.normalized seed
    plane = PC.Plane kx ky kz seed

truncateModelAtPoint :: Opaque (PC.FacedModel Float) -> Float -> Float -> IO (Opaque (PC.FacedModel Float))
truncateModelAtPoint model theta phi = return $ toOpaque m'
  where
    m' = PC.cutModel 0.00001 (PC.Plane nx ny nz sfPt) $ fromOpaque model
    sfPt@(G.Point3f nx ny nz) = GF.latLongPosition theta phi 1

toFlatModel :: Opaque (PC.FacedModel Float) -> IO ([Float], [Float], [Float], [Int], [Int], Float)
toFlatModel model = return (vs, ns, cs, ids, vpf, span)
  where
   FlatModel vs ns cs ids vpf span = fromModel $ PC.toModel fm
   fm = fromOpaque model

updateViewMat :: Float -> Float -> Float -> IO [Float]
updateViewMat t p d =
  return $ V.matToList $ GF.viewMatOf t p d

naiveRotMat :: Float -> Float -> IO [Float]
naiveRotMat t p =
  return $ V.matToList $ GF.naiveRotMat t p

orthoMatrixFromScreen :: Int -> Int -> IO [Float]
orthoMatrixFromScreen w h = return $ V.matToList $ G.orthoMatrixFromScreen w h

main = do
  export (toJSStr "naiveRotMat") naiveRotMat
  export (toJSStr "orthoMatrixFromScreen") orthoMatrixFromScreen
  export (toJSStr "updateViewMat") updateViewMat
  export (toJSStr "loadModel") loadModel
  export (toJSStr "truncate") truncateRotatedModel
  export (toJSStr "truncateAtPoint") truncateModelAtPoint
  export (toJSStr "toFlatModel") toFlatModel
