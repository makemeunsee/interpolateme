module Geometry ( Point3f(Point3f)
                , norm, normalized, cross, times, pointToArr, add, forceNorm, vec
                , gold
                , vec3
                , lookAtMatrix
                , orthoMatrix
                , multMat
                , rotate
                , facesToFlatIndice
                , facesToFlatTriangles
                , axisRndFacesToFlatTriangles
                , facesToCenterFlags
                , defaultSeed
                )
where

import qualified Data.Vec as V
import Data.Word
import Random.MWC.Pure
import ListUtil

data Point3f = Point3f Float Float Float


-- model conversion functions


-- flatten faces into an indice list.
-- faces are tessellated into triangles, each having the face barycenter as first point.
-- indice used in the parameter and result WILL NOT match.
-- to use along 'facesToFlatTriangles'.
-- eg: [7 9 8] -> [0 1 2 0 2 3 0 3 1] where 0 <- index of barycenter of the triangle
--                                          1 <- 7
--                                          2 <- 9
--                                          3 <- 8
facesToFlatIndice :: [[Int]] -> [Int]
facesToFlatIndice faces = facesToFlatIndice0 0 faces
  where
    facesToFlatIndice0 :: Int -> [[Int]] -> [Int]
    facesToFlatIndice0 _ [] = []
    facesToFlatIndice0 count (arr:arrs) = offsetTriangles ++ facesToFlatIndice0 (count+l+1) arrs
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
facesToFlatTriangles :: [Point3f] -> [[Int]] -> [Float]
facesToFlatTriangles _ [] = []
facesToFlatTriangles pts (arr:arrs) =
  (concatMap pointToArr (faceBarycenter pts arr : map ((!!) pts) arr)) ++ facesToFlatTriangles pts arrs


axisRndFacesToFlatTriangles :: Seed -> Point3f -> [Point3f] -> [[Int]] -> ([Float], Seed)
axisRndFacesToFlatTriangles seed _ _ [] = ([], seed)
axisRndFacesToFlatTriangles seed axis pts (arr:arrs) =
  ((concatMap pointToArr rndFace) ++ rndRemainder, finalSeed)
  where (rndFace, newSeed) = randomizeVerticeAlongAxis seed axis (faceBarycenter pts arr : map ((!!) pts) arr)
        (rndRemainder, finalSeed) = axisRndFacesToFlatTriangles newSeed axis pts arrs


-- flags vertice which are a barycenter and not part of the original face.
-- each face gets 1 barycenter and as many normal points as it originally contains.
-- to use along 'facesToFlatIndice' and 'facesToFlatTriangles'.
facesToCenterFlags :: [[Int]] -> [Float]
facesToCenterFlags [] = []
facesToCenterFlags (arr:arrs) =
  1 : (take (length arr) $ iterate id 0) ++ facesToCenterFlags arrs


-- using vertex data and a face, creates the barycenter of this face
faceBarycenter :: [Point3f] -> [Int] -> Point3f
faceBarycenter pts faceIds = barycenter $ map (pts !!) faceIds


-- average on a point3f list
barycenter :: [Point3f] -> Point3f
barycenter = uncurry (divBy) . foldr (\e (c,s) -> (c+1, e `add` s)) (0, Point3f 0 0 0)


-- randomize face functions


defaultSeed :: Seed
defaultSeed = seed $ map charToWord32 "defaultSeed"
 where charToWord32 c = fromIntegral $ fromEnum c


-- randomize 'depth' of face along given axis.
-- random from -5 to 5, 0.05 steps
randomizeVerticeAlongAxis :: Seed -> Point3f -> [Point3f] -> ([Point3f], Seed)
randomizeVerticeAlongAxis seed axis face = (translatedFace, newSeed)
  where nAxis = normalized axis
        originalDistToOrigin = head face `dot` nAxis
        translatedFace = map (add (times alpha $ nAxis)) face
        alpha = k / 15.0
        (k, newSeed) = range_random (0, 100 * signum originalDistToOrigin) seed


-- basic geometry functions


pointToArr :: Point3f -> [Float]
pointToArr (Point3f x y z) = [x,y,z]


-- the golden ratio aka tau aka phi
gold :: Float
gold = (1+sqrt 5)/2


norm :: Point3f -> Float
norm p = sqrt $ p `dot` p


add :: Point3f -> Point3f -> Point3f
add (Point3f x0 y0 z0) (Point3f x1 y1 z1) = Point3f (x0 + x1) (y0 + y1) (z0 + z1)


times :: Float -> Point3f -> Point3f
times a (Point3f x y z) = Point3f (x * a) (y * a) (z * a)


divBy :: Float -> Point3f -> Point3f
divBy a (Point3f x y z) = Point3f (x / a) (y / a) (z / a)


vec :: Point3f -> Point3f -> Point3f
vec (Point3f x0 y0 z0) (Point3f x1 y1 z1) = Point3f (x1-x0) (y1-y0) (z1-z0)


dist :: Point3f -> Point3f -> Float
dist (Point3f x0 y0 z0) (Point3f x1 y1 z1) = sqrt $ (x1-x0)*(x1-x0) + (y1-y0)*(y1-y0) + (z1-z0)*(z1-z0)


cross :: Point3f -> Point3f -> Point3f
cross (Point3f x0 y0 z0) (Point3f x1 y1 z1) = Point3f (y0 * z1 - z0 * y1) (z0 * x1 - x0 * z1) (x0 * y1 - y0 * x1)


dot :: Point3f -> Point3f -> Float
dot (Point3f x0 y0 z0) (Point3f x1 y1 z1) = x0*x1 + y0*y1 + z0*z1


type Normal = Point3f

normalized :: Point3f -> Normal
normalized p@(Point3f x y z) = Point3f (x / n) (y / n) (z / n)
  where n = norm p


forceNorm :: Float -> Point3f -> Point3f
forceNorm a p = a `times` (normalized p)


rotate :: Float -> Normal -> Point3f -> Point3f
rotate a n (Point3f x y z) =
  Point3f (m !! 0 !! 0 * x + m !! 0 !! 1 * y + m !! 0 !! 2 * z)
          (m !! 1 !! 0 * x + m !! 1 !! 1 * y + m !! 1 !! 2 * z)
          (m !! 2 !! 0 * x + m !! 2 !! 1 * y + m !! 2 !! 2 * z)
  where m = rotMatrix a n


rotMatrix :: Float -> Normal -> [[Float]]
rotMatrix a (Point3f nx ny nz) = [ [cos a + nx*nx*(1-cos a),    nx*ny*(1-cos a) - nz*sin a, nx*nz*(1-cos a) + ny*sin a]
                                 , [nx*ny*(1-cos a) + nz*sin a, cos a + ny*ny*(1-cos a),    ny*nz*(1-cos a) - nx*sin a]
                                 , [nx*nz*(1-cos a) - ny*sin a, ny*nz*(1-cos a) + nx*sin a, cos a + nz*nz*(1-cos a)]
                                 ]

lookAtMatrix :: Floating a => V.Vec3 a -> V.Vec3 a -> V.Vec3 a -> V.Mat44 a
lookAtMatrix eye target up = x V.:. y V.:. z V.:. h V.:. ()
	where
	forward = V.normalize $ target - eye
	right = V.normalize $ V.cross forward up
	up' = V.cross right forward
	x = V.snoc right (-(V.dot right eye))
	y = V.snoc up' (-(V.dot up' eye))
	z = V.snoc (-forward) (V.dot forward eye)
	h = 0 V.:. 0 V.:. 0 V.:. 1 V.:. ()


orthoMatrix :: Floating a => a -> a -> a -> a -> a -> a -> V.Mat44 a
orthoMatrix left right bottom top near far = x V.:. y V.:. z V.:. h V.:. ()
  where x_orth = 2 / (right - left)
        y_orth = 2 / (top - bottom)
        z_orth = -2 / (far - near)
        tx = -(right + left) / (right - left)
        ty = -(top + bottom) / (top - bottom)
        tz = -(far + near) / (far - near)
        x = x_orth V.:. 0 V.:. 0 V.:. tx V.:. ()
        y = 0 V.:. y_orth V.:. 0 V.:. ty V.:. ()
        z = 0 V.:. 0 V.:. z_orth V.:. tz V.:. ()
        h = 0 V.:. 0 V.:. 0 V.:. 1 V.:. ()


multMat :: Floating a => V.Mat44 a -> V.Mat44 a -> V.Mat44 a
multMat = V.multmm


vec3 x y z = (x V.:. y V.:. z V.:. ())