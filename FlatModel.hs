module FlatModel ( facesToFlatTriangles
                 , facesToCenterFlags
                 , facesToFlatIndice
                 , normalsToFlatNormals
                 , applyTranslationsToVertice
                 , FlatModel (FlatModel), FlatModel.vertice, normals, centers, indice, verticePerFace, FlatModel.span
                 , fromModel
) where

import Geometry ( Model (Model)
                , norm
                , Point3f (Point3f)
                , pointToArr
                , faceBarycenter
                , normalized
                , barycenter
                )
import ListUtil


data FlatModel a b = FlatModel { vertice :: [a]
                               , normals :: [a]
                               , centers :: [a]
                               , indice :: [b]
                               , verticePerFace :: [Int]
                               , span :: a }


fromModel :: (RealFloat a, Integral b) => Model a -> FlatModel a b
fromModel (Model vs fs ns) = FlatModel (facesToFlatTriangles vs fs)
                                       (normalsToFlatNormals ns fs)
                                       (facesToCenterFlags fs)
                                       (facesToFlatIndice fs)
                                       (map vCount fs)
                                       (if vs == [] then 1 else maximum $ map norm vs)
  where vCount face = let l = length face in
                      if length face == 3 then 3   -- no additional points for triangles
                                          else l+1 -- 1 barycenter added to larger poly faces


-- function to convert data from a Model to flat data, with a new center vertex added to each face of the model


applyTranslationsToVertice :: RealFloat a => [a] -> a -> a -> a -> [a] -> [Int] -> [a]
applyTranslationsToVertice tFactors xTAxis yTAxis zTAxis vs vpf =
  recurse vs $ zip tFactors vpf
  where recurse [] [] = []
        recurse vs ((r, count) : rfs) = translated ++ recurse rValues rfs
          where floatCount = 3*count
                values = take floatCount vs
                rValues = drop floatCount vs
                -- closer faces move closer, farther faces move farther -> minimize overlapping faces
                alpha = r * (signum r) * (signum $ xTAxis * baryX + yTAxis * baryY + zTAxis * baryZ)
                (baryX, baryY, baryZ) = if count == 3 -- compute barycenters for triangles
                                          then ( values !! 0 + values !! 3 + values !! 6
                                               , values !! 1 + values !! 4 + values !! 7
                                               , values !! 2 + values !! 5 + values !! 8
                                               )
                                          else (values !! 0, values !! 1, values !! 2)
                translation = take floatCount $ cycle [alpha*xTAxis, alpha*yTAxis, alpha*zTAxis]
                translated = map (\(v,t) -> v+t) $ zip values translation


-- flatten faces into an indice list.
-- faces are tessellated into triangles, each having the face barycenter as first point.
-- indice used in the parameter and result WILL NOT match.
-- to use along 'facesToFlatTriangles'.
-- eg: [7 9 8] -> [0 1 2 0 2 3 0 3 1] where 0 <- index of barycenter of the triangle
--                                          1 <- 7
--                                          2 <- 9
--                                          3 <- 8
facesToFlatIndice :: Integral a => [[Int]] -> [a]
facesToFlatIndice faces = facesToFlatIndice0 0 faces
  where
    facesToFlatIndice0 :: Integral a => a -> [[Int]] -> [a]
    facesToFlatIndice0 _ [] = []
    facesToFlatIndice0 count ([i0,i1,i2]:arrs) = [count,count+1,count+2] ++ facesToFlatIndice0 (count+3) arrs
    facesToFlatIndice0 count (arr:arrs) = offsetTriangles ++ facesToFlatIndice0 (1+count+ fromIntegral l) arrs
      where
        l = length arr
        triangles = firstWithEveryPair $ take (l+1) [0..]
        offsetTriangles = map (count+) triangles


-- concat triplets made of the input head, and each consecutive pair in the cycling input tail
firstWithEveryPair :: [a] -> [a]
firstWithEveryPair [] = []
firstWithEveryPair (f:r) = concatMap (\ (i,j) -> [f,i,j]) $ cyclicConsecutivePairs r


-- use vertex data (pts) and face data to create flat, tessellated vertex data
-- eg: [p0, p1, p2] [[0,2,1]] -> [ b.x, b.y, b.z, p0.x, p0.y, p0.z, p2.x, p2.y, p2.z,
--                                 b.x, b.y, b.z, p2.x, p2.y, p2.z, p1.x, p1.y, p1.z,
--                                 b.x, b.y, b.z, p1.x, p1.y, p1.z, p0.x, p0.y, p0.z ]
-- where b = (p0 + p1 + p2) / 3 (barycenter)
-- to use along 'facesToFlatIndice'.
facesToFlatTriangles :: RealFloat a => [Point3f a] -> [[Int]] -> [a]
facesToFlatTriangles _ [] = []
-- dont subdivide triangles
facesToFlatTriangles pts ([i0,i1,i2]:arrs) = (concatMap pointToArr $ map ((!!) pts) [i0,i1,i2]) ++ facesToFlatTriangles pts arrs
-- subdivide quads and larger polys into triangles with a center
facesToFlatTriangles pts (arr:arrs) =
  (concatMap pointToArr (faceBarycenter pts arr : map ((!!) pts) arr)) ++ facesToFlatTriangles pts arrs


-- convert 1 normal by face to 1 normal by vertex if necessary,
-- add 1 normal for the face center vertex
normalsToFlatNormals :: RealFloat a => [Point3f a] -> [[Int]] -> [a]
normalsToFlatNormals normals faces = if length normals == length faces
                                       then flatFaceNormals normals faces
                                       else flatVertexNormals normals faces


-- assign each face normal to each vertex of the face
-- 4 for a triangle (because a center vertex is added), 5 for quads, etc.
flatFaceNormals :: RealFloat a => [Point3f a] -> [[Int]] -> [a]
flatFaceNormals _ [] = []
-- triangles dont have the additional barycenter
flatFaceNormals (n:ns) ([i0,i1,i2] : faces) = (concatMap pointToArr $ replicate 3 n) ++ flatFaceNormals ns faces
-- larger polys have a barycenter
flatFaceNormals (n:ns) (face : faces) = (concatMap pointToArr $ replicate (1 + length face) n) ++ flatFaceNormals ns faces


-- add 1 normal to each face, for the added vertex (center)
flatVertexNormals :: RealFloat a => [Point3f a] -> [[Int]] -> [a]
flatVertexNormals _ [] = []
-- triangle face have 3 normals
flatVertexNormals normals (face@[_,_,_]:faces) =
  concatMap pointToArr faceNormals ++ facesToFlatTriangles normals faces
  where faceNormals = map (normals !!) face
-- larger poly faces need 1 more normal for the barycenter
flatVertexNormals normals (face:faces) =
  concatMap pointToArr (newNormal : faceNormals) ++ facesToFlatTriangles normals faces
  where faceNormals = map (normals !!) face
        newNormal = normalized $ barycenter faceNormals


-- barycentric coords for vertice
-- triangle vertice get (1,0,0) (0,1,0), (0,0,1)
-- even sided faces get (0,0,0) for their center, and alternating (1,0,0), (0,1,0) for their vertice
-- odd sided faces get (0,0,0) for their center, (0,0,1) for their first vertice and alternating (1,0,0), (0,1,0) for their other vertice
-- to use along 'facesToFlatIndice' and 'facesToFlatTriangles'.
facesToCenterFlags :: RealFloat a => [[Int]] -> [a]
facesToCenterFlags [] = []
-- triangle vertice barycentric coords
facesToCenterFlags (triangle@[_,_,_]:arrs) = 1 : 0 : 0 : 0 : 1 : 0 : 0 : 0 : 1 : facesToCenterFlags arrs
-- larger poly barycentric coords
facesToCenterFlags (arr:arrs) =
  1 : 1 : 1 : barycentrics ++ facesToCenterFlags arrs
  where barycentrics = if l `mod` 2 == 0
                         then bs
                         else 0 : 0 : 1 : bs
        l = length arr
        bs = take ((*) 6 $ l `quot` 2) $ cycle [1,0,0,0,1,0]