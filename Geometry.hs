{-# LANGUAGE RecordWildCards #-}

module Geometry ( Point3f(Point3f), Normal
                , origin
                , norm, normalized, cross, times, pointToArr, add, forceNorm, vec, divBy, dot
                , dist
                , Model(Model), vertice, faces
                , barycenter, faceBarycenter
                , edgeNeighbours, vertexNeighbours, facesForEachVertex
                , combine
                , gold
                , modelAutoNormals
                )
where

import Data.List (elemIndex, elemIndices)
import ListUtil

data Point3f a = Point3f a a a
                 deriving (Eq, Show)


origin :: RealFloat a => Point3f a
origin = Point3f 0 0 0

data Model a = Model { vertice :: [Point3f a], faces :: [[Int]], normals :: [Normal a] }
               deriving (Eq, Show)

-- neighbour functions look at indice, not vertice

-- list edge neighbours of each face (neighbour = sharing at least 2 consecutive vertice)
edgeNeighbours :: Model a -> [[Int]]
edgeNeighbours = genericNeighboursFct shareTwoConsecutiveVertice
  where
    -- look for matching pairs of vertice among the 2 faces (regardless of face orientation)
    shareTwoConsecutiveVertice f0 f1 = any (\p -> (any (p ==) pairs1) || any (p ==) revPairs1) pairs0
      where pairs0 = cyclicConsecutivePairs f0
            pairs1 = cyclicConsecutivePairs f1
            revPairs1 = cyclicConsecutivePairs $ reverse f1


vertexNeighbours :: Model a -> [[Int]]
vertexNeighbours = genericNeighboursFct shareOneVertex
  where
    shareOneVertex f0 f1 = any (\p -> (any (p ==) f1)) f0


genericNeighboursFct :: ([Int] -> [Int] -> Bool) -> Model a -> [[Int]]
genericNeighboursFct neighbourTestFct Model{..}  = map myNeighbours faces
  where
    -- retrieve indice of faces which are neighbours of a face
    myNeighbours face = elemIndices True $ map (areNeighbours face) faces
    -- 2 faces are neighbours if they share a vertice (and are not identical)
    areNeighbours f0 f1 = f0 /= f1 && neighbourTestFct f0 f1


-- for each vertex in a model, build a list of face (indice list) the vertex is part of
facesForEachVertex :: Model a -> [[Int]]
facesForEachVertex Model{..} = map (\i -> elemIndices True $ map (elem i) faces ) $ take (length vertice) [0..]


-- combine 2 models
combine :: RealFloat a => Model a -> Model a -> Model a
combine (Model v0 f0 n0) (Model v1 f1 n1) =
  Model (v0 ++ v1) (f0 ++ offset) (n0 ++ n1)
  where offset = map (map (idCount0 +)) f1
        idCount0 = length v0


-- provide a normal for each vertex = normalized vector (origin -> vertex)
modelAutoNormals :: RealFloat a => [Point3f a] -> [[Int]] -> Model a
modelAutoNormals vs fs = Model vs fs ns
  where ns = map normalized vs


-- using vertex data and a face, creates the barycenter of this face
faceBarycenter :: RealFloat a => [Point3f a] -> [Int] -> Point3f a
faceBarycenter pts faceIds = barycenter $ map (pts !!) faceIds


-- average on a point3f list
barycenter :: RealFloat a => [Point3f a] -> Point3f a
barycenter = uncurry (divBy) . foldr (\e (c,s) -> (c+1, e `add` s)) (0, Point3f 0 0 0)


-- basic geometry functions


pointToArr :: RealFloat a => Point3f a -> [a]
pointToArr (Point3f x y z) = [x,y,z]


-- the golden ratio aka tau aka phi
gold :: RealFloat a => a
gold = (1+sqrt 5)/2


norm :: RealFloat a => Point3f a -> a
norm p = sqrt $ p `dot` p


add :: RealFloat a => Point3f a -> Point3f a -> Point3f a
add (Point3f x0 y0 z0) (Point3f x1 y1 z1) = Point3f (x0 + x1) (y0 + y1) (z0 + z1)


times :: RealFloat a => a -> Point3f a -> Point3f a
times a (Point3f x y z) = Point3f (x * a) (y * a) (z * a)


divBy :: RealFloat a => a -> Point3f a -> Point3f a
divBy a (Point3f x y z) = Point3f (x / a) (y / a) (z / a)


vec :: RealFloat a => Point3f a -> Point3f a -> Point3f a
vec (Point3f x0 y0 z0) (Point3f x1 y1 z1) = Point3f (x1-x0) (y1-y0) (z1-z0)


dist :: RealFloat a => Point3f a -> Point3f a -> a
dist (Point3f x0 y0 z0) (Point3f x1 y1 z1) = sqrt $ (x1-x0)*(x1-x0) + (y1-y0)*(y1-y0) + (z1-z0)*(z1-z0)


cross :: RealFloat a => Point3f a -> Point3f  a-> Point3f a
cross (Point3f x0 y0 z0) (Point3f x1 y1 z1) = Point3f (y0 * z1 - z0 * y1) (z0 * x1 - x0 * z1) (x0 * y1 - y0 * x1)


dot :: RealFloat a => Point3f a -> Point3f a -> a
dot (Point3f x0 y0 z0) (Point3f x1 y1 z1) = x0*x1 + y0*y1 + z0*z1


type Normal = Point3f

normalized :: RealFloat a => Point3f a -> Normal a
normalized p@(Point3f x y z) = Point3f (x / n) (y / n) (z / n)
  where n = norm p


forceNorm :: RealFloat a => a -> Point3f a -> Point3f a
forceNorm a p = a `times` (normalized p)