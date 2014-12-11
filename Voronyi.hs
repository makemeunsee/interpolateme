module Voronyi

where

import Control.Exception (assert)
import qualified Geometry as G
import Data.List ( elemIndex )
import Data.Maybe ( fromJust )

-- data model for a voronoi tessellation of a sphere of radius 1
-- seeds are points on the sphere
-- vertice are the actual points of the tessellation
-- a polygon exists for each seed, each polygon is a list of vertex indice
data VoronoiModel a = VoronoiModel { seeds :: [G.Point3f a]
                                   , vertice :: [G.Point3f a]
                                   , polygons :: [[Int]]
                                   }


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


-- the seed must be on the plane
segmentPlaneIntersection :: RealFloat a => G.Point3f a -> G.Point3f a -> Plane a -> G.Point3f a -> Maybe (G.Point3f a)
segmentPlaneIntersection p0@(G.Point3f x0 y0 z0) p1 pl@(Plane a b c d) (G.Point3f sx sy sz) =
  if dx*a + dy*b +dz*c == 0
    then Nothing -- segment and plane parallel (or segment on plane)
    else if at >=0 && at <= 1
      then Just $ G.add p0 $ G.times at v
      else Nothing
  where v@(G.Point3f dx dy dz) = G.add p1 $ G.times (-1) p0 -- p0p1 vector
        at = (a*(sx-x0) + b*(sy-y0) + c*(sz-z0)) / (a*dx + b*dy + c*dz)


addSeed :: RealFloat a => G.Point3f a -> VoronoiModel a -> VoronoiModel a
addSeed newSeed@(G.Point3f x y z) m@(VoronoiModel ss vs ps) =
  -- find closest seed
  let closest = closestSeed m newSeed in
  -- intersect tangent plane at newSeed with closest seed walls (edges of faces)
  let tangent = tangentPlane newSeed in
  let walls = wallsOfSeed m (fromJust $ elemIndex closest ss) in

  -- get 2 points i0 and i1
  -- update polygon of closest seed
  -- from neighbour seed @ i0 to neighbour seed @ i1
  --   find new intersection points based on previous (starting at i0), until reaching i1
  --   update polygons
  m
