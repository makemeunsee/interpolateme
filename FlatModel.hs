module FlatModel ( facesToFlatIndice
                 , FlatModel (FlatModel), FlatModel.vertice, normals, centers, indice
                 , fromModel
) where

import Geometry ( Model (Model)
                , pointToArr
                , faceBarycenter
                , times
                )
import ListUtil


data FlatModel a b = FlatModel { vertice :: [a]
                               , normals :: [a]
                               , centers :: [a]
                               , indice :: [b]
                               }


fromModel :: (RealFloat a, Integral b) => Model a -> FlatModel a b
fromModel (Model vs fs ns) = FlatModel flatVs
                                       flatNormals
                                       centerFlags
                                       (facesToFlatIndice fs offset)
  where offset = length fs

        barycenters = map (faceBarycenter vs) fs
        edgeVs = concatMap (concatMap (\i -> [vs !! i, times 0.5 $ vs !! i])) fs
        flatVs = concatMap pointToArr (barycenters ++ edgeVs)

        baryNormals = map (\f -> ns !! head f) fs
        edgeNormals = concatMap (concatMap (\i -> [ns !! i, ns !! i])) fs
        flatNormals = concatMap pointToArr (baryNormals ++ ns)

        l = length edgeVs
        centerFlags = (take (length fs) $ repeat 1) ++ (take l $ repeat 0)


facesToFlatIndice :: Integral a => [[Int]] -> Int -> [a]
facesToFlatIndice faces baseOffset = facesToFlatIndice0 faces 0 baseOffset
  where
    facesToFlatIndice0 [] _ _ = []
    facesToFlatIndice0 (f:fs) baryId offset = (toTriangles baryId f offset) ++ facesToFlatIndice0 fs (baryId+1) (offset+2*length f)
    toTriangles baryId f offset = concatMap (\(i,j) ->  baryId : map (fromIntegral . (+) offset) [2*i,2*j,2*j,2*i,2*i+1,2*j,2*i+1,2*j+1]) $ cyclicConsecutivePairs $ take (length f) [0..]