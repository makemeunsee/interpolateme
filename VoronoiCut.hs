{-# LANGUAGE RecordWildCards #-}

module VoronoiCut ( fromModel
                  , toModel
                  , VoronoiModel (..)
                  , Face (..)
                  , closestFaceCenter
                  , cutFace
                  )

where

import Data.List (findIndices)

import Data.Sequence (Seq, index, fromList)
import qualified Data.Foldable as F
import Data.Maybe (fromJust, maybe)

import ListUtil
import qualified Geometry as G
import PlaneCut ( ToPlane (..)
                , Plane (..)
                , intersectPlaneAndSegment
                )


data Face a = Face { center :: G.Point3f a
                   , vertice :: [G.Point3f a]
                   , neighbours :: [Int]
                   }
              deriving (Eq, Show)


data VoronoiModel a = VoronoiModel { faces :: Seq (Face a) }
                      deriving (Eq, Show)


fromModel :: RealFloat a => G.Model a -> VoronoiModel a
fromModel m@(G.Model vs fs ns) = m'
  where
    -- scale vertice, so that the distance from face centers to origin is 1
    center0 = G.faceBarycenter vs $ fs !! 0
    scale = map (G.divBy $ G.norm center0)
    vs' = scale vs
    center f = G.normalized $ foldr1 G.add $ map (vs' !!) f
    faces = zip (map (\f -> (center f, map (vs' !!) f)) fs) (G.edgeNeighbours m)
    m' = VoronoiModel $ fromList $ map (\((c,vs),ns) -> Face c vs ns) faces


toModel :: RealFloat a => VoronoiModel a -> G.Model a
toModel VoronoiModel{..} = G.modelAutoNormals (reverse vs) fs
  where
    (vs, fs, _) = foldr (\f (vs, fs, offset) ->
                          ( (reverse $ vertice f) ++ vs                             -- prepend reversed shorter list, reverse everything once at the end
                          , (map (offset+) $ take (length $ vertice f) [0..]) : fs  --
                          , offset + (length $ vertice f))                          --
                        )
                        ([], [], 0)
                        $ F.toList faces


closestFaceCenter :: RealFloat a => VoronoiModel a -> G.Point3f a -> (Int, G.Point3f a)
closestFaceCenter vm@VoronoiModel{..} unitSpherePoint = closestRec 0 where
  faceCenter = center . index faces
  faceNeighbours = neighbours . index faces
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
                              $ neighbours $ index faces i in
    if (closerId == i) then
      (closerId, c)
    else
      closestRec closerId


tolerance :: RealFloat a => a
tolerance = 1e-8


cutFace :: RealFloat a => Face a -> Plane a -> (Face a, [G.Point3f a])
cutFace f@Face{..} pl@Plane{..} = ( Face center (cyclicRemoveConsecutiveDuplicates $ fst afterCut) neighbours
                                  , snd afterCut)
  where
    cutInfo = map (\v -> (v, toPlane v)) vertice
    afterCut = foldr (\(vi0, vi1) (allVerts, newVerts) -> case (vi0, vi1) of
                       ((v0,OnPlane),(v1,OnPlane)) -> (v0 : v1 : allVerts, newVerts) -- both on the plane, keep them
                       ((v0,OnPlane),(v1,Above))   -> (v0 : allVerts, newVerts)      -- only keep the one on the plane
                       ((v0,Above),(v1,OnPlane))   -> (v1 : allVerts, newVerts)      -- only keep the one on the plane
                       ((_,Above),(_,Above))       -> (allVerts, newVerts)           -- both out, discard both
                       ((v0,Below),(v1,Above))     -> -- cut
                         let newV = fromJust $ intersectPlaneAndSegment tolerance pl (v0,v1) in
                         (v0 : newV : allVerts, newV : newVerts)
                       ((v0,Above),(v1,Below))     -> -- cut
                         let newV = fromJust $ intersectPlaneAndSegment tolerance pl (v0,v1) in
                         (newV : v1 : allVerts, newV : newVerts)
                       ((v0,Below),(v1,_))         -> (v0 : v1 : allVerts, newVerts)  -- keep both
                       ((v0,_),(v1,Below))         -> (v0 : v1 : allVerts, newVerts)  -- keep both
                     )
                     ([], [])
                     $ cyclicConsecutivePairs cutInfo
    toPlane p = let (G.Point3f cx cy cz) = G.add p $ G.times (-1) seed in
                case cx*kx+cy*ky+cz*kz of x | abs x <= tolerance -> OnPlane
                                            | x < -tolerance     -> Below
                                            | otherwise          -> Above

