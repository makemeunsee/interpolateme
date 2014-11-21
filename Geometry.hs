module Geometry where

import qualified Data.Vec as V

data Point3f = Point3f Float Float Float


pointToArr :: Point3f -> [Float]
pointToArr (Point3f x y z) = [x,y,z]


-- the golden ratio aka tau aka phi
gold :: Float
gold = (1+sqrt 5)/2


norm :: Point3f -> Float
norm (Point3f x y z) = sqrt(x*x + y*y + z*z)


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