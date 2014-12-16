module Voronyi

where

import Control.Exception (assert)
import qualified Geometry as G
import ListUtil
import Data.List ( elemIndex, elemIndices, findIndex, findIndices )
import Data.Maybe ( fromJust, catMaybes, mapMaybe )

-- data model for a voronoi tessellation of a sphere of radius 1
-- seeds are points on the sphere
-- vertice are the actual points of the tessellation
-- a polygon exists for each seed, each polygon is a list of vertex indice
data VoronoiModel a = VoronoiModel { seeds :: [G.Point3f a]
                                   , vertice :: [G.Point3f a]
                                   , polygons :: [[Int]]
                                   }
                      deriving (Eq, Show)


-- 3d plane, its points (x,y,z) solutions to: a.x + b.y + c.z + d = 0
data Plane f = Plane { kx :: f, ky :: f, kz :: f, k0 :: f }
               deriving (Eq, Show)


onPlane :: RealFloat a => Plane a -> G.Point3f a -> Bool
onPlane (Plane a b c d) (G.Point3f x y z) = a*x + b*y + c*z + d == 0


---- for planes to which the origin does not belong (ie: k0 plane /= 0)
--intersectionOfPlanes :: RealFloat a => Plane a -> Plane a -> Maybe (Line a)
--intersectionOfPlanes (Plane a0 b0 c0 d0) (Plane a1 b1 c1 d1) =
--  let a0' = a0 / d0 in
--  let b0' = b0 / d0 in
--  let c0' = c0 / d0 in
--  let a1' = a1 / d1 in
--  let b1' = b1 / d1 in
--  let c1' = c1 / d1 in
--  if a0' == a1' && b0' == b1' && c0' == c1'
--    then Nothing -- planes are identical or parallel
--    else Just $ Line ( (b0*c1 + b1*c0)/f ) ( (b0*d1 + b1*d0)/f ) ( (a0*c1 + a1*c0)/f ) ( (a0*d1 + a1*d0)/f )
--  where f = a0*b1 + a1*b0


-- naive transposition of a model to a voronoi model
-- face barycenters become seeds
-- the vertice are scaled so that the seed of the first face belongs to the the unit sphere
toVoronoiModel :: RealFloat a => G.Model a -> VoronoiModel a
toVoronoiModel (G.Model vs fs _) = VoronoiModel (scale ss) (scale vs) fs
  where ss = map (G.faceBarycenter vs) fs
        scale = map (G.divBy $ G.norm $ ss !! 0)


fromVoronoiModel :: RealFloat a => VoronoiModel a -> G.Model a
fromVoronoiModel vModel = G.modelAutoNormals (vertice vModel) (polygons vModel)


-- find the closest seed of a voronoi model to the projection of a point on the unit sphere
closestSeed :: RealFloat a => VoronoiModel a -> G.Point3f a -> G.Point3f a
closestSeed (VoronoiModel ss _ _) p = foldr1 closer ss
  where projected = G.normalized p
        closer p0 p1 = if projected `G.dist` p0 < projected `G.dist` p1
                         then p0
                         else p1


wallsOfSeed :: RealFloat a => VoronoiModel a -> Int -> [G.Point3f a]
wallsOfSeed m@(VoronoiModel ss vs ps) i = assert (i > -1 && i < length ss)
  map (vs !!) (ps !! i)


medianPlane :: RealFloat a => G.Point3f a -> G.Point3f a -> Plane a
medianPlane p0@(G.Point3f x0 y0 z0) p1@(G.Point3f x1 y1 z1) = Plane vx vy vz (-vx*mx-vy*my-vz*mz)
  where (vx,vy,vz) = ((x1-x0), (y1-y0), (z1-z0))
        (mx,my,mz) = ((x1+x0)/2, (y1+y0)/2, (z1+z0)/2)


-- plane tanget to unit sphere with normal vector v
tangentPlane :: RealFloat a => G.Point3f a -> Plane a
tangentPlane v@(G.Point3f x y z) = Plane nx ny nz (-1)
  where (G.Point3f nx ny nz) = G.normalized v


data Intersection a = OnPoint (G.Point3f a)
                    | OnSegment (G.Point3f a)
                    | None
                    deriving (Show, Eq)


getPoint :: RealFloat a => Intersection a -> Maybe (G.Point3f a)
getPoint (OnPoint p) = Just p
getPoint (OnSegment p) = Just p
getPoint _ = Nothing


-- the seed must be on the plane
segmentPlaneIntersection :: RealFloat a => G.Point3f a -> G.Point3f a -> Plane a -> G.Point3f a -> Intersection a
segmentPlaneIntersection p0@(G.Point3f x0 y0 z0) p1 pl@(Plane a b c d) (G.Point3f sx sy sz) =
  if dx*a + dy*b +dz*c == 0
    then None -- segment and plane parallel (or segment on plane)
    else
      if at < 0 then None
      else if at == 0 then OnPoint p0
      else if at == 1 then OnPoint p1
      else if at > 1 then None
      else OnSegment $ G.add p0 $ G.times at v
  where v@(G.Point3f dx dy dz) = G.add p1 $ G.times (-1) p0 -- p0p1 vector
        at = (a*(sx-x0) + b*(sy-y0) + c*(sz-z0)) / (a*dx + b*dy + c*dz)


cutPolygon :: RealFloat a => [G.Point3f a] -> Plane a -> G.Point3f a -> [G.Point3f a]
cutPolygon vertice plane@(Plane a b c d) planeSeed = map (newVertice !!) newFace
  where
    l = length vertice
    ids = take l [0..]
    segments = cyclicConsecutivePairs vertice
    intersections = map (\(p0,p1) -> segmentPlaneIntersection p0 p1 plane planeSeed) segments
    withIds = filter (\(i, _) -> i /= None) $ zip intersections [0..]
    (newVertice, newFace) = case withIds of

      -- cutting the polygon at 2 edges
      [(OnPoint p0, i0), (OnPoint p1, i1), (OnPoint p2, i2), (OnPoint p3, i3)]
        -- generic case
        | p0 == p1 && p2 == p3 && i0 + 1 == i1 && i2 + 1 == i3 ->
          let kept = if underPlane $ vertice !! 0 then [0..i1] ++ [i3..l-1] else [i1..i3] in
          (vertice, kept)
        -- cutting 2 edges special case: one edge is first vertex
        | p0 == p3 && p1 == p2 && i0 == 0 && i3 == l - 1 && i1 + 1 == i2 ->
          let kept = if underPlane $ vertice !! i1 then [0..i2] else 0:[i2..i3] in
          (vertice, kept)
        -- should not happen
        | otherwise ->
         assert False ([], [])

      -- cutting 1 segment then 1 edge
      [(OnSegment p0, i0), (OnPoint p1, i1), (OnPoint p2, i2)]
        | p1 == p2 && i1 + 1 == i2 ->
          let kept = if underPlane $ vertice !! 0 then [0..i0] ++ l:[i2..l-1] else l:[i0+1..i2] in
          (vertice ++ [p0], kept)
        -- should not happen
        | otherwise ->
          assert False ([], [])

      -- cutting 1 edge then 1 segment
      [(OnPoint p0, i0), (OnPoint p1, i1), (OnSegment p2, i2)]
        | p0 == p1 && i0 + 1 == i1 ->
          let kept = if underPlane $ vertice !! 0 then [0..i1] ++ l:[i2+1..l-1] else l:[i1..i2] in
          (vertice ++ [p2], kept)
        -- should not happen
        | otherwise ->
          assert False ([], [])

      -- cutting 1 edge and 1 segment, special case: edge is first vertex
      [(OnPoint p0, i0), (OnSegment p1, i1), (OnPoint p2, i2)]
        | p0 == p2 && i0 == 0 && i2 == l - 1 ->
          let kept = if underPlane $ vertice !! i1 then l:[0..i1] else 0:l:[i1+1..l-1] in
          (vertice ++ [p1], kept)
        -- should not happen
        | otherwise ->
          assert False ([], [])

      -- cutting the polygon on 2 segments
      [(OnSegment p0, i0), (OnSegment p1, i1)] ->
        let kept = if underPlane $ vertice !! 0 then [0..i0] ++ [l,l+1] ++ [i1+1..l-1] else l+1:l:[i0+1..i1] in
        (vertice ++ [p0,p1], kept)
      -- no intersection case: leave polygon intact
      [] ->
        (vertice, ids)
      -- default case: should not happen
      _ ->
        assert False ([], [])

    underPlane v = let (G.Point3f cx cy cz) = G.add v $ G.times (-1) planeSeed in
                   cx*a+cy*b+cz*c < 0


truncate :: RealFloat a => G.Point3f a -> VoronoiModel a -> VoronoiModel a
truncate normal@(G.Point3f x y z) m@(VoronoiModel ss vs fs) =
  buildFromPolygons newFaces ss
  where
    -- using the tangent plane with the given normal
    tangent = tangentPlane normal
    oldFaces = map (map (vs !!)) fs
    -- cut all faces
    newFaces = map (\f -> cutPolygon f tangent normal) oldFaces


buildFromPolygons :: RealFloat a => [[G.Point3f a]] -> [G.Point3f a] -> VoronoiModel a
buildFromPolygons polygons normals = VoronoiModel normals (reverse reducedVertice) $ reverse $ map reverse faces
  where
    -- Note: all lists are built backward then reversed
    (reducedVertice, faces) = reduce polygons ([], [])
    reduce [] acc = acc
    reduce (polygon:ps) (vs, fs) = reduce ps $ mergePolygon polygon (vs, []:fs)
    -- merge polygon insert polygon vertice in the vertice list if not already in
    -- and fill the head of the face list with the polygon vertice id in the updated list
    mergePolygon [] acc = acc
    mergePolygon (p:ps) (vs, fs) = mergePolygon ps newAcc
      where
        f = head fs
        l = length vs
        newAcc = case elemIndex p vs of
                   Nothing -> let f = head fs in
                              -- append the vertice as a new vertice to the list
                              ( p : vs, ( l : f) : ( tail fs ) )
                   Just id -> let f = head fs in
                              -- dont append the vertice, refer to the existing copy in the face
                              ( vs, ( ( (l-id-1) : f ) : ( tail fs ) ) )
