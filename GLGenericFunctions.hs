module GLGenericFunctions

where

import Data.Vec (Mat44)
import Data.Vec.LinAlg (NearZero)
import Geometry

-- interpolation mimicking that of polyhedra.vert
interpolate :: RealFloat a => a -> [a] -> [a] -> [a]
interpolate t from to = map interp $ zip from to
  where alpha = 0.5 + 0.5 * cos t
        interp (f0,f1) = alpha*f0 + (1-alpha)*f1

-- camera and matrice related functions


data OrbitingState a = OrbitingState { theta :: a    -- angle between x axis and the orbiting point projected on the xz plane
                                     , phi :: a      -- angle between y axis and the orbiting point
                                     , distance :: a -- orbiting distance
                                     , thetaSpeed :: a -- angular speed for theta variations
                                     , phiSpeed :: a   -- angular speed for phi variations
                                     }


-- direction to origin from orbiting position
orbitCenterDirection :: RealFloat a => OrbitingState a -> Point3f a
orbitCenterDirection orbit = (-1) `times` (orbitingPosition orbit)


-- orbiting point position
orbitingPosition :: RealFloat a => OrbitingState a -> Point3f a
orbitingPosition orbit = Point3f x y z
  where (x,y,z) = ( d * sin (phi orbit) * cos (theta orbit)
                  , d * cos (phi orbit)
                  , d * sin (phi orbit) * sin (theta orbit))
        d = distance orbit


orbitingEyeForModel :: (RealFloat a, NearZero a) => Mat44 a -> OrbitingState a -> Point3f a
orbitingEyeForModel modelMatrix orbit = vec4ToPoint3f orbitEye
  where orbitEye = multInvMatV modelMatrix $ vec4 ex ey ez
        Point3f ex ey ez = orbitCenterDirection orbit


viewMatOf :: RealFloat a => OrbitingState a -> Mat44 a
viewMatOf orbit@OrbitingState { phi = p } = newViewMat
  where (Point3f px py pz) = orbitingPosition orbit { phi = max 0.00001 $ min (pi-0.00001) p }
        newViewMat = lookAtMatrix (vec3 px py pz)
                                  (vec3 0 0 0)
                                  (vec3 0 1 0)


modelMatOf :: RealFloat a => OrbitingState a -> Mat44 a
modelMatOf OrbitingState { theta = t, phi = p } = yMat `multMat` xzMat
  where xzMat = rotMatrix4 t (Point3f 0 1 0)
        yMat = rotMatrix4 (p-pi/2) $ rotate t (Point3f 0 1 0) (Point3f 0 0 1)


limitAngle :: (Floating a, Ord a) => a -> a
limitAngle angle =
  if angle < 0
    then 0
    else if angle > pi
      then pi
      else angle


updateOrbitAngles :: RealFloat a => a -> a -> OrbitingState a -> OrbitingState a
updateOrbitAngles diffTheta diffPhi orbit =
  orbit { theta = (theta orbit) + diffTheta
        , phi = limitAngle $ (phi orbit) + diffPhi
        }

