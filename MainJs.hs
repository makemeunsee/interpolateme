{-# LANGUAGE RecordWildCards #-}

import Models ( icosahedron )
import Labyrinth
import qualified VoronoiCut as VC
import qualified LinAlgFunctions as LAF
import qualified Geometry as G
import qualified Data.Vec as V
import RandomUtil

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


createMazePlanet :: String -> Int -> Int -> IO ( [Float], [Float], [Float], [Float], [Int] )
createMazePlanet seedStr cellCount overlapThreshold = do
  let seed = seedForString seedStr
  let baseModel = VC.fromModel icosahedron
  let (cuts, seed') = generateRndCuts cellCount seed
  putStrLn $ show cuts
  let cutModel = foldr (\(t,p) m -> VC.cutModelFromAngles t p m) baseModel $ reverse cuts
  let (laby, seed'') = labyrinth1 seed' overlapThreshold $ VC.faces cutModel
  let (depths, maxDepth) = depthMap laby
  let (vertexBuffer, ids, centerBuffer, mazeBuffer, normalBuffer) = toBufferData (VC.faces cutModel) depths maxDepth
  return (concatMap G.pointToArr vertexBuffer, concatMap G.pointToArr normalBuffer, centerBuffer, mazeBuffer, ids)


main = do
  export (toJSStr "naiveRotMat") naiveRotMat
  export (toJSStr "orthoMatrixFromScreen") orthoMatrixFromScreen
  export (toJSStr "updateViewMat") updateViewMat
  export (toJSStr "createMazePlanet") createMazePlanet
