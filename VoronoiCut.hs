{-# LANGUAGE RecordWildCards #-}

module VoronoiCut ( fromModel
                  , VoronoiModel (..)
                  , normals
                  , faceList
                  , faceCount
                  , Face (..)
                  , barycenter
                  , closestSeed
                  , cutFace
                  , cutModelFromAngles
                  , cutModel
                  )

where


import Data.List (findIndices, groupBy, find, partition)

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Set (singleton, notMember, insert)
import qualified Data.Foldable as F
import Data.Maybe (fromJust, maybe)
import qualified Data.List as L

import ListUtil
import qualified Geometry as G
import qualified LinAlgFunctions as LAF
import PlaneCut ( ToPlane (..)
                , Plane (..)
                , intersectPlaneAndSegment
                )


data Face a = Face { seed :: !(G.Point3f a)
                   , vertice :: ![G.Point3f a]
                   , neighbours :: ![Int]
                   }
              deriving (Eq, Show)


barycenter :: RealFloat a => Face a -> G.Point3f a
barycenter Face{..} = G.barycenter vertice


data VoronoiModel a = VoronoiModel { faces :: !(Seq (Face a)) }
                      deriving (Eq, Show)


faceCount :: VoronoiModel a -> Int
faceCount = S.length . faces


faceList :: RealFloat a => VoronoiModel a -> [Face a]
faceList vm = F.toList $ faces vm


normals :: RealFloat a => VoronoiModel a -> [G.Point3f a]
normals = (map seed) . faceList


fromModel :: RealFloat a => G.Model a -> VoronoiModel a
fromModel m@(G.Model vs fs ns) = m'
  where
    -- scale vertice, so that the distance from face centers to origin is 1
    center0 = G.faceBarycenter vs $ fs !! 0
    scale = map (G.divBy $ G.norm center0)
    vs' = scale vs
    center f = G.normalized $ foldr1 G.add $ map (vs' !!) f
    faces = zip (map (\f -> (center f, map (vs' !!) f)) fs) (G.edgeNeighbours m)
    m' = VoronoiModel $ S.fromList $ map (\((c,vs),ns) -> Face c vs ns) faces


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


closestSeed :: RealFloat a => VoronoiModel a -> G.Point3f a -> (Int, G.Point3f a)
closestSeed vm@VoronoiModel{..} unitSpherePoint = closestRec 0 where
  faceSeed = seed . S.index faces
  faceNeighbours = neighbours . S.index faces
  closestRec i =
    let c = faceSeed i in
    let (closerId, _) = foldr (\j' (j, d) ->
                                let d' = G.dist unitSpherePoint $ faceSeed j' in
                                if d' < d then
                                  (j', d')
                                else
                                  (j, d)
                              )
                              (i, G.dist unitSpherePoint c)
                              $ neighbours $ S.index faces i in
    if (closerId == i) then
      (closerId, c)
    else
      closestRec closerId


tolerance :: RealFloat a => a
tolerance = 1e-8


cutFace :: RealFloat a => Int -> Face a -> Plane a -> (Face a, [G.Point3f a])
cutFace newFaceId f@Face{..} pl@Plane{..} =
  ( Face seed (cyclicRemoveConsecutiveDuplicates $ fst afterCut) newNeighbours
  , edgePoints)
  where
    edgePoints = removeDups $ snd afterCut
    newNeighbours = if length edgePoints > 0 then (newFaceId:neighbours) else neighbours
    cutInfo = map (\v -> (v, toPlane v)) vertice
    afterCut = foldr (\(vi0, vi1) (allVerts, newVerts) -> case (vi0, vi1) of
                       ((v0,OnPlane),(v1,OnPlane)) -> (v0 : v1 : allVerts, v0 : v1 : newVerts) -- both on the plane, both on edge.
                       ((v0,OnPlane),(v1,Above))   -> (v0 : allVerts, v0 : newVerts)           -- only keep the one on the plane
                       ((v0,Above),(v1,OnPlane))   -> (v1 : allVerts, v1 : newVerts)           -- only keep the one on the plane
                       ((_,Above),(_,Above))       -> (allVerts, newVerts)                     -- discard both, no cut edge
                       ((v0,Below),(v1,Above))     -> -- cut, keep first and create one, new one on cut edge
                         let newV = fromJust $ intersectPlaneAndSegment tolerance pl (v0,v1) in
                         (v0 : newV : allVerts, newV : newVerts)
                       ((v0,Above),(v1,Below))     -> -- cut, keep second and create one, new one on cut edge
                         let newV = fromJust $ intersectPlaneAndSegment tolerance pl (v0,v1) in
                         (newV : v1 : allVerts, newV : newVerts)
                       ((v0,Below),(v1,Below))     -> (v0 : v1 : allVerts, newVerts)       -- keep both, no cut edge
                       ((v0,Below),(v1,OnPlane))   -> (v0 : v1 : allVerts, v1 : newVerts)  -- keep both, first on cut edge
                       ((v0,OnPlane),(v1,Below))   -> (v0 : v1 : allVerts, v0 : newVerts)  -- keep both, second on cut edge
                     )
                     ([], [])
                     $ cyclicConsecutivePairs cutInfo
    toPlane p = let (G.Point3f cx cy cz) = G.add p $ G.times (-1) ptOfPl in
                case cx*kx+cy*ky+cz*kz of x | abs x <= tolerance -> OnPlane
                                            | x < -tolerance     -> Below
                                            | otherwise          -> Above


cutModelFromAngles :: RealFloat a => a -> a -> VoronoiModel a -> VoronoiModel a
cutModelFromAngles theta phi m = cutModel m (Plane nx ny nz sfPt)
  where sfPt@(G.Point3f nx ny nz) = LAF.latLongPosition theta phi 1



cutModel :: RealFloat a => VoronoiModel a -> Plane a -> VoronoiModel a
cutModel vm@VoronoiModel{..} p@Plane{..} = VoronoiModel $ updatedFaces |> newFace
  where
    planeNormal = G.Point3f kx ky kz
    newFaceId = S.length faces
    -- find the closest face to cut
    (firstCutFaceId, _) = closestSeed vm ptOfPl
    -- cut a face and try to cuts its neighbours
    cuts = doCuts [firstCutFaceId] (singleton firstCutFaceId) []

    uFaces = map (\(i,f,_) -> (i,f)) cuts
    updatedFaces = foldr (\(i, f) fs -> S.adjust (\_ -> f) i fs) faces (cleanNeighbours uFaces)

    -- extract points of the new face, order them properly
    newPoints = foldr (\(i,_,pts) allPts -> map (\p -> (p,i)) pts ++ allPts) [] cuts
    newPointsWithLoc = associate newPoints
    orderedPoints = maybeFlipPolygon planeNormal $ map fst $ chain newPointsWithLoc

    -- extract the face neighbours
    newFaceNeighbours = removeDups $ concatMap snd newPointsWithLoc

    newFace = Face ptOfPl orderedPoints newFaceNeighbours

    doCuts [] _ acc = acc
    doCuts (i:ids) visited acc =
      let f = S.index faces i in
      let (newFace, newPoints) = cutFace newFaceId f p in
      -- valid intersection of polygon edges and plane happens at only 2 points
      if length newPoints /= 2 then
        doCuts ids (insert i visited) acc
      else
        doCuts ( (filter (\j -> not (elem j ids) && notMember j visited) (neighbours f)) ++ ids)
               (insert i visited)
               ( (i, newFace, newPoints) : acc )


maybeFlipPolygon :: RealFloat a => G.Point3f a -> [G.Point3f a] -> [G.Point3f a]
maybeFlipPolygon up is@(p0:p1:p2:_) =
  let normal = (G.vec p0 p1) `G.cross` (G.vec p1 p2) in
  let k = normal `G.dot` up in
  if k > 0 then
    is
  else
    reverse is
maybeFlipPolygon _ is = is


chain [] = []
chain ((p,ids):r) = (p, ids) : (chain newR)
  where
    newR = if r == [] then
             []
           else
             left ++ right
    (left, right) = partition (\(_,ids') -> (==) 1 $ length $ intersection ids ids') r


cleanNeighbours [] = []
cleanNeighbours ((i,f):fs) = cleanedF : cleanNeighbours cleanedFs
  where
    (cleanedF, cleanedFs) = foldr (\(i'',f'') ((i',f'), fs') ->
                                    let ns' = neighbours f' in
                                    let vs' = vertice f' in
                                    let ns'' = neighbours f'' in
                                    let vs'' = vertice f'' in
                                    if elem i'' ns' then
                                      if (<=) 2 $ length $ filter (\v -> elem v vs'') vs' then
                                        ((i',f'), (i'',f'') : fs' )
                                      else
                                        ( (i', f' { neighbours = filter (/=i'') ns' })
                                        , (i'', f'' { neighbours = filter (/=i') ns'' }) : fs'
                                        )
                                    else
                                      ((i',f'), (i'',f'') : fs' )
                                  ) ((i,f), []) fs
