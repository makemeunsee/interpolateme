module FlatModel ( facesToFlatTriangles
                 , facesToCenterFlags
                 , facesToFlatIndice
                 , normalsToFlatNormals
                 , FlatModel (FlatModel), FlatModel.vertice, FlatModel.faces, normals, centers, indice, verticePerFace, FlatModel.span
                 , fromModel
) where

import Geometry
import ListUtil


data FlatModel a b c = FlatModel { vertice :: [a]
                                 , faces :: [Int]
                                 , normals :: [a]
                                 , centers :: [b]
                                 , indice :: [c]
                                 , verticePerFace :: [Int]
                                 , span :: a }


fromModel :: (RealFloat a, Integral b, Integral c) => Model a -> FlatModel a b c
fromModel (Model vs fs ns) = FlatModel (facesToFlatTriangles vs fs)
                                       (facesToFlatIndice fs)
                                       (normalsToFlatNormals ns fs)
                                       (facesToCenterFlags fs)
                                       (facesToFlatIndice fs)
                                       (map ((1 +) . length) fs) -- barycenter added to original faces
                                       (maximum $ map norm vs)


-- function to convert data from a Model to flat data, with a new center vertex added to each face of the model


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
flatFaceNormals (n:ns) (face : faces) = (concatMap pointToArr $ replicate (1 + length face) n) ++ flatFaceNormals ns faces


-- add 1 normal to each face, for the added vertex (center)
flatVertexNormals :: RealFloat a => [Point3f a] -> [[Int]] -> [a]
flatVertexNormals _ [] = []
flatVertexNormals normals (face:faces) =
  concatMap pointToArr (newNormal : faceNormals) ++ facesToFlatTriangles normals faces
    where faceNormals = map (normals !!) face
          newNormal = normalized $ barycenter faceNormals


-- flags vertice which are a barycenter and not part of the original face.
-- each face gets 1 barycenter and as many normal points as it originally contains.
-- to use along 'facesToFlatIndice' and 'facesToFlatTriangles'.
facesToCenterFlags :: Integral a => [[Int]] -> [a]
facesToCenterFlags [] = []
facesToCenterFlags (arr:arrs) =
  1 : (take (length arr) $ iterate id 0) ++ facesToCenterFlags arrs