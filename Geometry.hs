module Geometry ( Point3f(Point3f), Normal
                , norm, normalized, cross, times, pointToArr, add, forceNorm, vec
                , Model(Model), vertice, faces
                , combine
                , gold
                , vec3, vec4, vec4ToPoint3f
                , lookAtMatrix
                , orthoMatrix
                , scale
                , multMat, multInvMatV
                , rotate
                , rotateM, rotateL
                , negXRot, posXRot, negYRot, posYRot
                , barycenter
                , facesToFlatIndice
                , facesToFlatTriangles
                , facesToCenterFlags
                )
where

import qualified Data.Vec as V
import Data.Word
import Data.Maybe (fromJust)
import ListUtil

data Point3f a = Point3f a a a


data Model a = Model { vertice :: [Point3f a], faces :: [[Int]] }


-- combine 2 models
combine :: RealFloat a => Model a -> Model a -> Model a
combine (Model v0 f0) (Model v1 f1) =
  Model (v0 ++ v1) (f0 ++ offset)
  where offset = map (map (idCount0 +)) f1
        idCount0 = length v0


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
facesToFlatTriangles :: RealFloat a => [Point3f a] -> [[Int]] -> [a]
facesToFlatTriangles _ [] = []
facesToFlatTriangles pts (arr:arrs) =
  (concatMap pointToArr (faceBarycenter pts arr : map ((!!) pts) arr)) ++ facesToFlatTriangles pts arrs


-- flags vertice which are a barycenter and not part of the original face.
-- each face gets 1 barycenter and as many normal points as it originally contains.
-- to use along 'facesToFlatIndice' and 'facesToFlatTriangles'.
facesToCenterFlags :: RealFloat a => [[Int]] -> [a]
facesToCenterFlags [] = []
facesToCenterFlags (arr:arrs) =
  1 : (take (length arr) $ iterate id 0) ++ facesToCenterFlags arrs


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


rotate :: RealFloat a => a -> Normal a -> Point3f a -> Point3f a
rotate a n (Point3f x y z) =
  Point3f (m !! 0 !! 0 * x + m !! 0 !! 1 * y + m !! 0 !! 2 * z)
          (m !! 1 !! 0 * x + m !! 1 !! 1 * y + m !! 1 !! 2 * z)
          (m !! 2 !! 0 * x + m !! 2 !! 1 * y + m !! 2 !! 2 * z)
  where m = rotMatrix a n


rotateM :: RealFloat a => V.Mat44 a -> Point3f a -> Point3f a
rotateM mat (Point3f x y z) = Point3f rx ry rz
  where rx V.:. ry V.:. rz V.:. _ = V.multmv mat $ vec4 x y z


rotateL :: RealFloat a => [a] -> Point3f a -> Point3f a
rotateL l = rotateM $ V.matFromList l


rotMatrix :: RealFloat a => a -> Normal a -> [[a]]
rotMatrix a (Point3f nx ny nz) = [ [cos a + nx*nx*(1-cos a),    nx*ny*(1-cos a) - nz*sin a, nx*nz*(1-cos a) + ny*sin a]
                                 , [nx*ny*(1-cos a) + nz*sin a, cos a + ny*ny*(1-cos a),    ny*nz*(1-cos a) - nx*sin a]
                                 , [nx*nz*(1-cos a) - ny*sin a, ny*nz*(1-cos a) + nx*sin a, cos a + nz*nz*(1-cos a)]
                                 ]


lookAtMatrix :: RealFloat a => V.Vec3 a -> V.Vec3 a -> V.Vec3 a -> V.Mat44 a
lookAtMatrix eye target up = x V.:. y V.:. z V.:. h V.:. ()
	where
	forward = V.normalize $ target - eye
	right = V.normalize $ V.cross forward up
	up' = V.cross right forward
	x = V.snoc right (-(V.dot right eye))
	y = V.snoc up' (-(V.dot up' eye))
	z = V.snoc (-forward) (V.dot forward eye)
	h = 0 V.:. 0 V.:. 0 V.:. 1 V.:. ()


orthoMatrix :: RealFloat a => a -> a -> a -> a -> a -> a -> V.Mat44 a
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


multMat :: RealFloat a => V.Mat44 a -> V.Mat44 a -> V.Mat44 a
multMat = V.multmm


-- multInvMatV :: Floating a => V.Mat44 a -> V.Vec4 a -> V.Vec4 a
multInvMatV m = V.multmv (fromJust $ V.invert m)


scale :: RealFloat a => a -> V.Mat44 a -> V.Mat44 a
scale k = V.scale (vec4 k k k)


vec3 x y z = (x V.:. y V.:. z V.:. ())


vec4 x y z = (x V.:. y V.:. z V.:. 1 V.:. ())


vec4ToPoint3f (x V.:. y V.:. z V.:. _ V.:. ()) = Point3f x y z


negXRot :: RealFloat a => V.Mat44 a
negXRot = x V.:. y V.:. z V.:. w V.:. ()
  where x = 1 V.:. 0    V.:. 0    V.:. 0 V.:. ()
        y = 0 V.:. 0    V.:. 1    V.:. 0 V.:. ()
        z = 0 V.:. (-1) V.:. 0    V.:. 0 V.:. ()
        w = 0 V.:. 0    V.:. 0    V.:. 1 V.:. ()


posXRot :: RealFloat a => V.Mat44 a
posXRot = x V.:. y V.:. z V.:. w V.:. ()
  where x = 1 V.:. 0    V.:. 0    V.:. 0 V.:. ()
        y = 0 V.:. 0    V.:. (-1) V.:. 0 V.:. ()
        z = 0 V.:. 1    V.:. 0    V.:. 0 V.:. ()
        w = 0 V.:. 0    V.:. 0    V.:. 1 V.:. ()


negYRot :: RealFloat a => V.Mat44 a
negYRot = x V.:. y V.:. z V.:. w V.:. ()
  where x = 0 V.:. 0    V.:. (-1) V.:. 0 V.:. ()
        y = 0 V.:. 1    V.:. 0    V.:. 0 V.:. ()
        z = 1 V.:. 0    V.:. 0    V.:. 0 V.:. ()
        w = 0 V.:. 0    V.:. 0    V.:. 1 V.:. ()


posYRot :: RealFloat a => V.Mat44 a
posYRot = x V.:. y V.:. z V.:. w V.:. ()
  where x = 0    V.:. 0    V.:. 1    V.:. 0 V.:. ()
        y = 0    V.:. 1    V.:. 0    V.:. 0 V.:. ()
        z = (-1) V.:. 0    V.:. 0    V.:. 0 V.:. ()
        w = 0    V.:. 0    V.:. 0    V.:. 1 V.:. ()