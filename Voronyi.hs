module Voronyi

where

import Control.Exception (assert)
import qualified Geometry as G
import ListUtil
import Data.List ( elemIndex, elemIndices, findIndex, findIndices, null )
import Data.Maybe ( fromJust, catMaybes, mapMaybe )

import System.IO.Unsafe ( unsafePerformIO )

-- data model for a voronoi tessellation of a sphere of radius 1
-- seeds are points on the sphere
-- vertice are the actual points of the tessellation
-- neighbours are the indice of vertex neighbour faces (sharing at least one vertex)
-- a polygon exists for each seed, each polygon is a list of vertex indice
-- TODO replace neighbours with the list of all the faces touching at each vertex
data VoronoiModel a = VoronoiModel { seeds :: [G.Point3f a]
                                   , vertice :: [G.Point3f a]
                                   , polygons :: [[Int]]
                                   , neighbours :: [[Int]]
                                   }
                      deriving (Eq, Show)


-- 3d plane, its points (x,y,z) solutions to: a.x + b.y + c.z + d = 0
data Plane f = Plane { kx :: f, ky :: f, kz :: f, k0 :: f }
               deriving (Eq, Show)


onPlane :: RealFloat a => Plane a -> G.Point3f a -> Bool
onPlane (Plane a b c d) (G.Point3f x y z) = a*x + b*y + c*z + d == 0


-- naive transposition of a model to a voronoi model
-- face barycenters become seeds
-- the vertice are scaled so that the seed of the first face belongs to the the unit sphere
toVoronoiModel :: RealFloat a => G.Model a -> VoronoiModel a
toVoronoiModel m@(G.Model vs fs _) = VoronoiModel (scale ss) (scale vs) fs neighs
  where ss = map (G.faceBarycenter vs) fs
        scale = map (G.divBy $ G.norm $ ss !! 0)
        neighs = G.vertexNeighbours m


fromVoronoiModel :: RealFloat a => VoronoiModel a -> G.Model a
fromVoronoiModel vModel = G.modelAutoNormals (vertice vModel) (polygons vModel)


-- find the closest seed of a voronoi model to the projection of a point on the unit sphere
closestSeed :: RealFloat a => VoronoiModel a -> G.Point3f a -> G.Point3f a
closestSeed (VoronoiModel ss _ _ _) p = foldr1 closer ss
  where projected = G.normalized p
        closer p0 p1 = if projected `G.dist` p0 < projected `G.dist` p1
                         then p0
                         else p1


wallsOfSeed :: RealFloat a => VoronoiModel a -> Int -> [G.Point3f a]
wallsOfSeed m@(VoronoiModel ss vs ps _) i = assert (i > -1 && i < length ss)
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


clean :: RealFloat a => a -> [Intersection a] -> [Intersection a]
clean _ [] = []
clean _ [x] = [x]
clean tolerance (x0:x1:xs) = r0 : (clean tolerance $ r1 : xs)
  where
    (r0, r1) = case (x0,x1) of
      (OnPoint p0, OnSegment p1)
        | G.dist p0 p1 < tolerance -> (OnPoint p0, OnPoint p0)
        | otherwise -> (x0, x1)
      (OnSegment p0, OnPoint p1)
        | G.dist p0 p1 < tolerance -> (OnPoint p1, OnPoint p1)
        | otherwise -> (x0, x1)
      _ -> (x0,x1)


-- the seed must be on the plane
segmentPlaneIntersection :: RealFloat a => a -> G.Point3f a -> G.Point3f a -> Plane a -> G.Point3f a -> Intersection a
segmentPlaneIntersection tolerance p0@(G.Point3f x0 y0 z0) p1 pl@(Plane a b c d) (G.Point3f sx sy sz) =
  if tolerance > coplanarity
    then None -- segment and plane parallel (or segment on plane)
    else
      if abs position < tolerance then OnPoint p0
      else if abs (position - l) < tolerance then OnPoint p1
      else if at > 0 && at < 1 then OnSegment $ G.add p0 $ G.times at v
      else None
  where v@(G.Point3f dx dy dz) = G.add p1 $ G.times (-1) p0 -- p0p1 vector
        at = (a*(sx-x0) + b*(sy-y0) + c*(sz-z0)) / (a*dx + b*dy + c*dz)
        l = G.norm v
        position = at * l
        coplanarity = abs $ dx*a + dy*b +dz*c


-- cut a flat, convex polygon with a plane
-- vertice contains the points of the polygon to cut
-- planeSeed is a point of the cutting plane, which could be extracted from the plane equation but is not
-- returns the truncated part of the polygon 'under' the plane, and the points were the cut occurred (so 2 or 0)
cutPolygon :: RealFloat a => a -> [G.Point3f a] -> Plane a -> G.Point3f a -> ([G.Point3f a], [G.Point3f a])
cutPolygon tolerance vertice plane@(Plane a b c d) planeSeed = (map (newVertice !!) newFace, map (newVertice !!) $ reverse newEdges)
  where
    l = length vertice
    ids = take l [0..]
    segments = cyclicConsecutivePairs vertice
    intersections = map (\(p0,p1) -> segmentPlaneIntersection tolerance p0 p1 plane planeSeed) segments
    cleaned = clean tolerance intersections
    withIds = filter (\(i, _) -> i /= None) $ zip cleaned [0..]
    (newVertice, newFace, newEdges) = case withIds of

      -- cutting the polygon at 2 edges
      [(OnPoint p0, i0), (OnPoint p1, i1), (OnPoint p2, i2), (OnPoint p3, i3)]
        -- generic case
        | p0 == p1 && p2 == p3 && i0 + 1 == i1 && i2 + 1 == i3 ->
          let under = underPlane $ vertice !! 0 in
          let kept = if under then [0..i1] ++ [i3..l-1] else [i1..i3] in
          let newEdges = if under then [i1,i3] else [i3,i1] in
          (vertice, kept, newEdges)
        -- cutting 2 edges special case: one edge is first vertex
        | p0 == p3 && p1 == p2 && i0 == 0 && i3 == l - 1 && i1 + 1 == i2 ->
          let under = underPlane $ vertice !! i1 in
          let kept = if under then [0..i2] else 0:[i2..i3] in
          let newEdges = if under then [i2, 0] else [0,i2] in
          (vertice, kept, newEdges)
        -- should not happen
        | otherwise ->
         assert False ([], [], [])

      -- cutting 1 segment then 1 edge
      [(OnSegment p0, i0), (OnPoint p1, i1), (OnPoint p2, i2)]
        | p1 == p2 && i1 + 1 == i2 ->
          let under = underPlane $ vertice !! 0 in
          let kept = if under then [0..i0] ++ l:[i2..l-1] else l:[i0+1..i2] in
          let newEdges = if under then [l,i2] else [i2,l] in
          (vertice ++ [p0], kept, newEdges)
        -- should not happen
        | otherwise ->
          assert False ([], [], [])

      -- cutting 1 edge then 1 segment
      [(OnPoint p0, i0), (OnPoint p1, i1), (OnSegment p2, i2)]
        | p0 == p1 && i0 + 1 == i1 ->
          let under = underPlane $ vertice !! 0 in
          let kept = if under then [0..i1] ++ l:[i2+1..l-1] else l:[i1..i2] in
          let newEdges = if under then [i1,l] else [l,i1] in
          (vertice ++ [p2], kept, newEdges)
        -- should not happen
        | otherwise ->
          assert False ([], [], [])

      -- cutting 1 edge and 1 segment, special case: edge is first vertex
      [(OnPoint p0, i0), (OnSegment p1, i1), (OnPoint p2, i2)]
        | p0 == p2 && i0 == 0 && i2 == l - 1 ->
          let under = underPlane $ vertice !! i1 in
          let kept = if under then l:[0..i1] else 0:l:[i1+1..l-1] in
          let newEdges = if under then [l,0] else [0,l] in
          (vertice ++ [p1], kept, newEdges)
        -- should not happen
        | otherwise ->
          assert False ([], [], [])

      -- cutting the polygon on 2 segments
      [(OnSegment p0, i0), (OnSegment p1, i1)] ->
        let under = underPlane $ vertice !! 0 in
        let kept = if under then [0..i0] ++ [l,l+1] ++ [i1+1..l-1] else l+1:l:[i0+1..i1] in
        let newEdges = if under then [l,l+1] else [l+1,l] in
        (vertice ++ [p0,p1], kept, newEdges)
      -- no intersection case: leave polygon intact
      [] ->
        (vertice, ids, [])
      -- intersection with only 1 point: leave polygon intact
      [(OnPoint p0, i0), (OnPoint p1, i1)] ->
        (vertice, ids, [])
      -- default case: should not happen
      _ ->
        assert False ([], [], [])

    underPlane v = let (G.Point3f cx cy cz) = G.add v $ G.times (-1) planeSeed in
                   cx*a+cy*b+cz*c < 0


truncate :: RealFloat a => a -> G.Point3f a -> VoronoiModel a -> VoronoiModel a
truncate tolerance spherePt@(G.Point3f x y z) m@(VoronoiModel ss vs fs neighs) =
  -- a new face is created only if 3 or more edges were created by truncating
  if length newEdges <= 2
    then m
    else
      VoronoiModel (spherePt : ss) newVertice newIndexedFaces updatedNeighbours
  where
    -- using the tangent plane with the given normal
    tangent = tangentPlane spherePt
    oldFaces = map (map (vs !!)) fs
    -- cut all faces
    rawCuts = map (\f -> cutPolygon tolerance f tangent spherePt) oldFaces
    -- the existing faces, after the cut
    facesAfterCut = fst $ unzip rawCuts
    -- extract new edges created by the cut
    newEdges = map snd $ filter (not . null . snd) rawCuts
    -- assemble them into a face
    newFace = buildPolygonFromSegments newEdges
    -- clean vertice, rebuild faces
    (newVertice, newIndexedFaces) = reducePolygons $ newFace : facesAfterCut
    -- find new neighbours
    updatedNeighbours = neighs


buildPolygonFromSegments :: RealFloat a => [[G.Point3f a]] -> [G.Point3f a]
buildPolygonFromSegments segments = rebuild (tail segments) (head segments)
  where
    rebuild [] acc = tail acc -- prevent loop
    rebuild segments acc =
      let target = head acc in
      let nextSeg = fst $ foldr (\newSeg (seg, d) ->
                                   let newD = G.dist (last newSeg) target in
                                   if newD < d then (newSeg, newD) else (seg, d)
                                 ) (head segments, 9e99) segments in
      rebuild (foldr (\e acc -> if e /= nextSeg then (e:acc) else acc) [] segments) (nextSeg ++ tail acc)


-- from a list of polygons, gather distinct vertice into a list, transform polygons into lists of indice
reducePolygons :: RealFloat a => [[G.Point3f a]] -> ([G.Point3f a], [[Int]])
reducePolygons polygons = (reverse reducedVertice, reverse $ map reverse faces)
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
