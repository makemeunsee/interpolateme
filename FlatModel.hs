module FlatModel ( facesToFlatIndice
                 , FlatModel (FlatModel), FlatModel.vertice, normals, centers, indice
                 , fromModel
) where

import Geometry ( Model (Model)
                , pointToArr
                , faceBarycenter
                )
import ListUtil


data FlatModel a = FlatModel { vertice :: [a]
                             , normals :: [a]
                             , centers :: [a]
                             , indice :: [Int]
                             }


fromModel :: RealFloat a => Model a -> FlatModel a
fromModel (Model vs fs ns) = FlatModel flatVs
                                       flatNormals
                                       centerFlags
                                       (facesToFlatIndice fs offset)
  where l = length vs
        offset = length fs
        barycenters = map (faceBarycenter vs) fs
        flatVs = concatMap pointToArr (barycenters ++ vs)
        baryNormals = map (\f -> ns !! head f) fs
        flatNormals = concatMap pointToArr (baryNormals ++ ns)
        centerFlags = (take (length fs) $ repeat 1) ++ (take l $ repeat 0)


facesToFlatIndice :: [[Int]] -> Int -> [Int]
facesToFlatIndice faces offset = facesToFlatIndice0 faces 0
  where
    facesToFlatIndice0 [] _ = []
    facesToFlatIndice0 (f:fs) count = (toTriangles count f) ++ facesToFlatIndice0 fs (count+1)
    toTriangles baryId f = concatMap (\ (i,j) -> [baryId,i+offset,j+offset]) $ cyclicConsecutivePairs f