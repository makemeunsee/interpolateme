{-# LANGUAGE RecordWildCards #-}

import FlatModel ( FlatModel (..), fromModel )
import FloretSphere ( polyhedrons )
import Labyrinth
import qualified VoronoiCut as VC
import qualified PlaneCut as PC
import qualified GLGenericFunctions as GF
import qualified Geometry as G
import qualified Data.Vec as V

import Haste
import Haste.Prim
import Haste.Foreign


models :: [Opaque (VC.VoronoiModel Float)]
models = map (toOpaque . VC.fromModel) polyhedrons

loadModel :: Int -> IO (Opaque (VC.VoronoiModel Float))
loadModel modelId = return $ models !! modelId

truncateModelAtPoint :: Opaque (VC.VoronoiModel Float) -> Float -> Float -> IO (Opaque (VC.VoronoiModel Float))
truncateModelAtPoint model theta phi = return $ toOpaque m'
  where
    m' = VC.cutModel (fromOpaque model) $ PC.Plane nx ny nz sfPt
    sfPt@(G.Point3f nx ny nz) = GF.latLongPosition theta phi 1

toFlatModel :: Opaque (VC.VoronoiModel Float) -> IO ([Float], [Int], [Float], [Float])
toFlatModel model = return (concatMap G.pointToArr vs, ids, concatMap G.pointToArr ns, cs)
  where
   (vs, ids, cs, ns) = VC.toBufferData $ fromOpaque model

toMaze :: Opaque (VC.VoronoiModel Float) -> IO ( Opaque ( Labyrinth Int ) )
toMaze o_model = return $ toOpaque $ labyrinth1 $ VC.faces $ fromOpaque o_model

toMazeData :: Opaque (VC.VoronoiModel Float) -> Opaque (Labyrinth Int) -> IO [Float]
toMazeData o_model o_laby = return $ mazeData (fromOpaque o_laby) (fromOpaque o_model)

mazeVerticeAndIds :: Opaque (VC.VoronoiModel Float) -> Opaque (Labyrinth Int) -> IO ([Float], [Int])
mazeVerticeAndIds o_model o_laby = return ( concatMap G.pointToArr $ labyrinthToPathVertice (VC.faces $ fromOpaque o_model) (fromOpaque o_laby)
                                          , labyrinthToPathIndice 0 $ fromOpaque o_laby)

updateViewMat :: Float -> Float -> Float -> IO [Float]
updateViewMat t p d =
  return $ V.matToList $ GF.viewMatOf t p d

naiveRotMat :: Float -> Float -> IO [Float]
naiveRotMat t p =
  return $ V.matToList $ GF.naiveRotMat t p

orthoMatrixFromScreen :: Int -> Int -> IO [Float]
orthoMatrixFromScreen w h = return $ V.matToList $ G.orthoMatrixFromScreen w h 1.75

main = do
  export (toJSStr "naiveRotMat") naiveRotMat
  export (toJSStr "orthoMatrixFromScreen") orthoMatrixFromScreen
  export (toJSStr "updateViewMat") updateViewMat
  export (toJSStr "loadModel") loadModel
  export (toJSStr "truncateAtPoint") truncateModelAtPoint
  export (toJSStr "toFlatModel") toFlatModel
  export (toJSStr "toMaze") toMaze
  export (toJSStr "toMazeData") toMazeData
  export (toJSStr "mazeVerticeAndIds") mazeVerticeAndIds
