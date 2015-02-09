module LinAlgFunctions

where

import qualified Data.Vec as V
import Geometry
import Foreign.C.Types (CFloat)


-- NearZero instance for GLfloat
import Data.Vec.LinAlg (NearZero(..))
instance NearZero CFloat where
  nearZero x = abs x < 1e-6
  {-# INLINE nearZero #-}


-- matrice related functions


-- view matrix from point orbiting origin at spheric coords theta, phi, distance
viewMatOf :: RealFloat a => a -> a -> a -> V.Mat44 a
viewMatOf theta phi distance = newViewMat
  where (Point3f px py pz) = latLongPosition theta (max 0.00001 $ min (pi-0.00001) phi) distance
        newViewMat = lookAtMatrix (vec3 px py pz)
                                  (vec3 0 0 0)
                                  (vec3 0 1 0)


latLongPosition :: RealFloat a => a -> a -> a -> Point3f a
latLongPosition theta phi distance =
  Point3f x y z
  where (x,y,z) = ( distance * sin phi * cos theta
                  , distance * cos phi
                  , distance * sin phi * sin theta)


-- simple rotation matrix of angle theta around y axis, phi around x axis
naiveRotMat :: RealFloat a => a -> a -> V.Mat44 a
naiveRotMat theta phi = yMat `multMat` xzMat
  where xzMat = rotMatrix4 theta (Point3f 0 1 0)
        yMat = rotMatrix4 phi (Point3f 1 0 0)


rotMatrix :: RealFloat a => a -> Normal a -> [[a]]
rotMatrix a (Point3f nx ny nz) = [ [cos a + nx*nx*(1-cos a),    nx*ny*(1-cos a) - nz*sin a, nx*nz*(1-cos a) + ny*sin a]
                                 , [nx*ny*(1-cos a) + nz*sin a, cos a + ny*ny*(1-cos a),    ny*nz*(1-cos a) - nx*sin a]
                                 , [nx*nz*(1-cos a) - ny*sin a, ny*nz*(1-cos a) + nx*sin a, cos a + nz*nz*(1-cos a)]
                                 ]

rotMatrix4 :: RealFloat a => a -> Normal a -> V.Mat44 a
rotMatrix4 a p = V.matFromLists [m3 !! 0 ++ [0], m3 !! 1 ++ [0], m3 !! 2 ++ [0], [0,0,0,1]]
  where m3 = rotMatrix a p


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


orthoMatrixFromScreen :: (RealFloat a, Integral b) => b -> b -> a -> V.Mat44 a
orthoMatrixFromScreen w h k = orthoMatrix left right bottom top near far
  where hh = if h < 0 then 1 else h
        aspect = fromIntegral w / fromIntegral hh
        far = 100
        near = -100
        right = k * aspect
        top = k
        left = -right
        bottom = -top


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


projMatrixFromScreen :: (RealFloat a, Integral b) => b -> b -> V.Mat44 a
projMatrixFromScreen w h = projMatrix near far top (wf / hf)
  where wf = fromIntegral w
        hf = fromIntegral h
        near = 1
        far = 10
        top = 1


projMatrix :: RealFloat a => a -> a -> a -> a -> V.Mat44 a
projMatrix n f t aspectRatio = x V.:. y V.:. z V.:. h V.:. ()
  where r = t * aspectRatio
        x = (n/r) V.:. 0 V.:. 0 V.:. 0 V.:. ()
        y = 0 V.:. (n/t) V.:. 0 V.:. 0 V.:. ()
        z = 0 V.:. 0 V.:. (-(f+n)/(f-n)) V.:. (-2*f*n/(f-n)) V.:. ()
        h = 0 V.:. 0 V.:. (-1) V.:. 0 V.:. ()


multMat :: RealFloat a => V.Mat44 a -> V.Mat44 a -> V.Mat44 a
multMat = V.multmm


scale :: RealFloat a => a -> V.Mat44 a -> V.Mat44 a
scale k = V.scale (vec4 k k k)


vec3 x y z = x V.:. y V.:. z V.:. ()


vec4 x y z = x V.:. y V.:. z V.:. 1 V.:. ()
