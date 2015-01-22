{-# LANGUAGE RecordWildCards #-}

import Models ( icosahedron, polyhedrons )
import Labyrinth
import qualified VoronoiCut as VC
import qualified LinAlgFunctions as LAF
import qualified Geometry as G
import qualified Data.Vec as V
import RandomUtil
import ListUtil

import Haste
import Haste.Prim
import Haste.Foreign


updateViewMat :: Float -> Float -> Float -> IO [Float]
updateViewMat t p d =
  return $ V.matToList $ LAF.viewMatOf t p d


naiveRotMat :: Float -> Float -> IO [Float]
naiveRotMat t p =
  return $ V.matToList $ LAF.naiveRotMat t p


orthoMatrixFromScreen :: Int -> Int -> IO [Float]
orthoMatrixFromScreen w h = return $ V.matToList $ LAF.orthoMatrixFromScreen w h 1.75


bareModel :: Int -> IO ( [Float], [Float], [Float], [Float], [Int] )
bareModel i = do
  let seed = seedForString "seeds exceed excess diagonally"
  let m = VC.fromModel $ polyhedrons !! i
  let (laby, _) = labyrinth1 seed 100 $ VC.faces m
  let (depths, maxDepth) = depthMap laby
  let (vertexBuffer, ids, centerBuffer, mazeBuffer, normalBuffer) = toBufferData (VC.faces m) depths maxDepth
  return (concatMap G.pointToArr vertexBuffer, concatMap G.pointToArr normalBuffer, centerBuffer, mazeBuffer, ids)


createMazePlanet :: Int -> String -> [Float] -> Int -> IO ( [Float], [Float], [Float], [Float], [Int] )
createMazePlanet baseModelId seedStr cuts overlapThreshold = do
  let i = min 3 $ max 0 baseModelId
  let o = min 1 $ max 100 overlapThreshold
  let seed = seedForString seedStr
  let baseModel = VC.fromModel $ polyhedrons !! i
  let cutModel = foldr (\[t,p] m -> VC.cutModelFromAngles t p m) baseModel $ chop 2 cuts
  let (laby, _) = labyrinth1 seed o $ VC.faces cutModel
  let (depths, maxDepth) = depthMap laby
  let (vertexBuffer, ids, centerBuffer, mazeBuffer, normalBuffer) = toBufferData (VC.faces cutModel) depths maxDepth
  return (concatMap G.pointToArr vertexBuffer, concatMap G.pointToArr normalBuffer, centerBuffer, mazeBuffer, ids)


main = do
  export (toJSStr "naiveRotMat") naiveRotMat
  export (toJSStr "orthoMatrixFromScreen") orthoMatrixFromScreen
  export (toJSStr "updateViewMat") updateViewMat
  export (toJSStr "createMazePlanet") createMazePlanet
  export (toJSStr "bareModel") bareModel
