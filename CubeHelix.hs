module CubeHelix ( cubeHelixInterpolationFct
                 )

where


cubeHelixInterpolationFct :: RealFloat a => a -> a -> a -> a -> a -> a -> a -> a -> [a]
cubeHelixInterpolationFct gamma h0 s0 l0 h1 s1 l1 t = [ l + a * (-0.14861 * cosh + 1.78277 * sinh)
                                                      , l + a * (-0.29227 * cosh - 0.90649 * sinh)
                                                      , l + a * 1.97294 * cosh]
  where
    startH = (h0 + 120) * pi / 180
    diffH = (h1 + 120) * pi / 180 - startH
    diffS = s1 - s0
    diffL = l1 - l0
    h = startH + diffH * t
    l = (l0 + diffL * t) ** gamma
    a = (s0 + diffS * t) * l * (1 - l)
    cosh = cos h
    sinh = sin h