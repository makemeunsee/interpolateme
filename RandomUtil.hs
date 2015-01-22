module RandomUtil

where


import Foreign.C.Types (CFloat, CInt)
import qualified Random.MWC.Pure as RND


seedForString :: String -> RND.Seed
seedForString str = RND.seed $ map charToWord32 str
  where charToWord32 c = fromIntegral $ fromEnum c


-- GLfloat RangeRandom instance
instance RND.RangeRandom CFloat where
  range_random (x0, x1) s = (realToFrac r, s')
    where (r, s') = RND.range_random(realToFrac x0 :: Float, realToFrac x1 :: Float) s


-- GLint RangeRandom instance
instance RND.RangeRandom CInt where
  range_random (x0, x1) s = (fromIntegral r, s')
    where (r, s') = RND.range_random(fromIntegral x0 :: Int, fromIntegral x1 :: Int) s


rndSpherePosition :: (RealFloat a, RND.RangeRandom a) => RND.Seed -> (a, a, RND.Seed)
rndSpherePosition seed = (2*pi*u, acos $ 2*v - 1, newSeed)
  where
    (u, newSeed0) = RND.range_random (0, 1) seed
    (v, newSeed) = RND.range_random (0, 1) newSeed0


generateRndCuts :: (RealFloat a, RND.RangeRandom a) => Int -> RND.Seed -> ([(a,a)], RND.Seed)
generateRndCuts 0 seed = ([], seed)
generateRndCuts n seed
  | n <= 0    = ([], seed)
  | otherwise = ((theta, phi) : arr, seed'')
  where
    (arr, seed'') = generateRndCuts (n-1) seed'
    (theta, phi, seed') = rndSpherePosition seed