module GLGenericFunctions

where

import Data.Vec (Mat44)
import Data.Vec.LinAlg (NearZero)
import Geometry


-- camera and matrice related functions


-- orbiting point position
orbitingPosition :: RealFloat a => a -> a -> a -> Point3f a
orbitingPosition theta phi distance = Point3f x y z
  where (x,y,z) = ( distance * sin phi * cos theta
                  , distance * cos phi
                  , distance * sin phi * sin theta)


-- view matrix from point orbiting origin at spheric coords theta, phi, distance
viewMatOf :: RealFloat a => a -> a -> a -> Mat44 a
viewMatOf theta phi distance = newViewMat
  where (Point3f px py pz) = orbitingPosition theta (max 0.00001 $ min (pi-0.00001) phi) distance
        newViewMat = lookAtMatrix (vec3 px py pz)
                                  (vec3 0 0 0)
                                  (vec3 0 1 0)


latLongPosition :: RealFloat a => a -> a -> a -> Point3f a
latLongPosition theta phi distance =
  Point3f x y z
  where (x,y,z) = ( distance * sin (pi-phi) * cos (-theta)
                  , distance * cos (pi-phi)
                  , distance * sin (pi-phi) * sin (-theta))


-- simple rotation matrix of angle theta around y axis, phi around x axis
naiveRotMat :: RealFloat a => a -> a -> Mat44 a
naiveRotMat theta phi = (yMat `multMat` xzMat)
  where xzMat = rotMatrix4 theta (Point3f 0 1 0)
        yMat = rotMatrix4 phi (Point3f 1 0 0)