{-# LANGUAGE RecordWildCards #-}

module VoronoiCut ( fromModel
                  , toModel
                  , VoronoiModel (VoronoiModel)
                  , closestFaceCenter
                  )

where


import Data.List (findIndices)

import Data.Sequence (Seq, index, fromList)
import qualified Data.Foldable as F
import Data.Maybe (fromJust, maybe)

import qualified Geometry as G


type Vertex a = ( G.Point3f a, [Int] )


data ToPlane = Above | Below | OnPlane deriving (Eq, Show)


data VoronoiModel a = VoronoiModel { vers :: Seq (Maybe (Vertex a))
                                   , facs :: Seq (Maybe ([Int], [Int]))
                                   , cens :: Seq (Maybe (G.Point3f a))
                                   }
                      deriving (Eq, Show)


faceNeighbours vm i = maybe Nothing (Just .snd) $ index (facs vm) i
facePoints vm i = maybe Nothing (Just .fst) $ index (facs vm) i


fromModel :: RealFloat a => G.Model a -> VoronoiModel a
fromModel m@(G.Model vs fs ns) = m'
  where
    -- scale vertice, so that the distance from face centers to origin is 1
    center0 = G.faceBarycenter vs $ fs !! 0
    scale = map (G.divBy $ G.norm center0)
    vs' = scale vs
    m' = VoronoiModel (fromList $ map Just $ zip vs' $ G.facesForEachVertex m)
                      (fromList $ map Just $ zip fs $ G.edgeNeighbours m)
                      (fromList $ (map (\f -> Just $ G.normalized $ foldr1 G.add $ map (ns !!) f) fs))


toModel :: RealFloat a => VoronoiModel a -> G.Model a
toModel VoronoiModel{..} = G.modelAutoNormals vs fs
  where
    tmp = filter (\(_, m_v) -> m_v /= Nothing) $ zip [0..] $ F.toList vers
    vs = map (\p -> fst $ fromJust $ snd p) tmp
    fs = map (\m_f -> map (\i -> head $ findIndices (\(j,_) -> j == i) tmp) $ fst $ fromJust m_f) $ filter (/= Nothing) $ F.toList facs


closestFaceCenter :: RealFloat a => VoronoiModel a -> G.Point3f a -> (Int, G.Point3f a)
closestFaceCenter vm@VoronoiModel{..} unitSpherePoint = closestRec 0 where
  faceCenter = fromJust . index cens
  closestRec i =
    let c = faceCenter i in
    let (closerId, _) = foldr (\j' (j, d) ->
                                let d' = G.dist unitSpherePoint $ faceCenter j' in
                                if d' < d then
                                  (j', d')
                                else
                                  (j, d)
                              )
                              (i, G.dist unitSpherePoint c)
                              $ fromJust $ faceNeighbours vm i in
    if (closerId == i) then
      (closerId, c)
    else
      closestRec closerId
