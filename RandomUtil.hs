module RandomUtil

where


import           Foreign.C.Types (CFloat, CInt)
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


rndPair :: (RealFloat a, RND.RangeRandom a) => RND.Seed -> (a, a, RND.Seed)
rndPair seed = (u, v, newSeed)
  where
    (u, newSeed0) = RND.range_random (0, 1) seed
    (v, newSeed) = RND.range_random (0, 1) newSeed0


generatePairs :: (RealFloat a, RND.RangeRandom a) => Int -> RND.Seed -> ([(a,a)], RND.Seed)
generatePairs n seed = generatePairs0 (max 0 n) seed []
  where
    generatePairs0 0 seed acc = (acc, seed)
    generatePairs0 n seed acc = generatePairs0 (n-1) seed' $ (u, v) : acc
      where (u, v, seed') = rndPair seed
