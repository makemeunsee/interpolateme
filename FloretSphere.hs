module FloretSphere ( tetrahedron
                    , cube
                    , dodecahedron
                    , stubRhombicosidodecahedron
                    , rhombicosidodecahedron
                    , snubDodecahedron
                    , pentagonalHexecontahedron
                    , icosahedron
                    , snubRhombiMix
                    , thinFloret
                    , pentaFloret
                    , polyhedrons
                    )
where

import Control.Exception (assert)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import ListUtil
import Geometry
import Geometry (Point3f, Normal, Model (Model))


-- some polyhedrons created along the way...
tetrahedron :: RealFloat a => Model a
tetrahedron = Model tetrahedronPoints tetrahedronFaces
cube :: RealFloat a => Model a
cube = Model cubePoints cubeFaces
dodecahedron :: RealFloat a => Model a
dodecahedron = Model dodecahedronPoints dodecahedronFaces
stubRhombicosidodecahedron :: RealFloat a => Model a
stubRhombicosidodecahedron = Model stubRhombicosidodecahedronPoints rhombicosidodecahedronFaces
rhombicosidodecahedron :: RealFloat a => Model a
rhombicosidodecahedron = Model rhombicosidodecahedronPoints rhombicosidodecahedronFaces
snubDodecahedron :: RealFloat a => Model a
snubDodecahedron = Model snubDodecahedronPoints snubDodecahedronFaces
pentagonalHexecontahedron :: RealFloat a => Model a
pentagonalHexecontahedron = Model pentagonalHexecontahedronPoints pentagonalHexecontahedronFaces
icosahedron :: RealFloat a => Model a
icosahedron = Model icosahedronPoints icosahedronFaces
snubRhombiMix :: RealFloat a => Model a
snubRhombiMix = Model snubDodecahedronPoints rhombicosidodecahedronFaces
thinFloret :: RealFloat a => Model a
thinFloret = Model thinFloretPoints thinFloretFaces
pentaFloret :: RealFloat a => Model a
pentaFloret = Model pentaFloretPoints pentaFloretFaces


polyhedrons :: RealFloat a => [Model a]
polyhedrons = [ tetrahedron
              , cube
              , dodecahedron
              , stubRhombicosidodecahedron
              , rhombicosidodecahedron
              , snubDodecahedron
              , pentagonalHexecontahedron
              , icosahedron
              , snubRhombiMix
              , thinFloret
              , pentaFloret
              ]


-- polyhedron geometry functions


-- tetrahedron


tetrahedronPoints :: RealFloat a => [Point3f a]
tetrahedronPoints = [ Point3f 1 1 1
                    , Point3f (-1) 1 (-1)
                    , Point3f 1 (-1) (-1)
                    , Point3f (-1) (-1) 1
                    ]


tetrahedronFaces :: [[Int]]
tetrahedronFaces = [ [0,2,1]
                   , [0,1,3]
                   , [0,3,2]
                   , [1,2,3]
                   ]


-- cube


cubePoints :: RealFloat a => [Point3f a]
cubePoints = [ Point3f 1 1 1
             , Point3f (-1) 1 1
             , Point3f (-1) 1 (-1)
             , Point3f 1 1 (-1)
             , Point3f (-1) (-1) (-1)
             , Point3f 1 (-1) (-1)
             , Point3f 1 (-1) 1
             , Point3f (-1) (-1) 1
             ]


cubeFaces :: [[Int]]
cubeFaces = [ [0,3,2,1]
            , [4,5,6,7]
            , [0,6,5,3]
            , [1,2,4,7]
            ]


-- Dodecahedron


-- vertice of a dodecahedron of edge length 2/gold
dodecahedronPoints :: RealFloat a => [Point3f a]
dodecahedronPoints = [ Point3f 1 1 1
                     , Point3f 1 1 (-1)
                     , Point3f 1 (-1) 1
                     , Point3f 1 (-1) (-1)
                     , Point3f (-1) 1 1
                     , Point3f (-1) 1 (-1)
                     , Point3f (-1) (-1) 1
                     , Point3f (-1) (-1) (-1)

                     , Point3f 0 (1/gold) gold
                     , Point3f 0 (1/gold) (-gold)
                     , Point3f 0 (-1/gold) gold
                     , Point3f 0 (-1/gold) (-gold)

                     , Point3f (1/gold) gold 0
                     , Point3f (1/gold) (-gold) 0
                     , Point3f (-1/gold) gold 0
                     , Point3f (-1/gold) (-gold) 0

                     , Point3f gold 0 (1/gold)
                     , Point3f gold 0 (-1/gold)
                     , Point3f (-gold) 0 (1/gold)
                     , Point3f (-gold) 0 (-1/gold)
                     ]


dodecahedronFaces :: [[Int]]
dodecahedronFaces = [ [12,0,16,17,1] -- 0
                    , [0,8,10,2,16]  -- 5
                    , [14,12,1,9,5]  -- 10
                    , [14,5,19,18,4] -- 15
                    , [18,19,7,15,6] -- 20
                    , [7,11,3,13,15] -- 25
                    , [6,15,13,2,10] -- 30
                    , [2,13,3,17,16] -- 35
                    , [7,19,5,9,11]  -- 40
                    , [0,12,14,4,8]  -- 45
                    , [8,4,18,6,10]  -- 50
                    , [1,17,3,11,9]  -- 55
                    ]


-- Rhombicosidodecahedron


-- triangles which tips each touch the tip of a pentagon
rhombicosidodecahedronTriangles :: [[Int]]
rhombicosidodecahedronTriangles = [ [1,45,5]
                                  , [4,55,12]
                                  , [8,33,35]
                                  , [27,57,37]
                                  , [19,51,48]
                                  , [14,42,16]
                                  , [24,30,53]
                                  , [22,40,25]
                                  , [6,49,50]
                                  , [13,59,43]
                                  , [7,54,34]
                                  , [26,44,58]
                                  , [0,11,46]
                                  , [28,36,32]
                                  , [10,15,47]
                                  , [23,29,31]
                                  , [2,9,39]
                                  , [3,38,56]
                                  , [18,20,52]
                                  , [17,41,21]
                                  ]


-- quads joining each 2 pentagons
rhombicosidodecahedronQuads :: [[Int]]
rhombicosidodecahedronQuads =
  [ [45,1,0,46]
  , [5,45,49,6]
  , [1,5,9,2]
  , [55,4,3,56]
  , [12,55,59,13]
  , [4,12,11,0]
  , [33,8,7,34]
  , [35,33,32,36]
  , [8,35,39,9]
  , [57,27,26,58]
  , [37,57,56,38]
  , [27,37,36,28]
  , [51,19,18,52]
  , [48,51,50,49]
  , [19,48,47,15]
  , [42,14,13,43]
  , [16,42,41,17]
  , [14,16,15,10]
  , [30,24,23,31]
  , [53,30,34,54]
  , [24,53,52,20]
  , [40,22,21,41]
  , [25,40,44,26]
  , [22,25,29,23]
  , [6,50,54,7]
  , [43,59,58,44]
  , [46,11,10,47]
  , [28,32,31,29]
  , [2,39,38,3]
  , [20,18,17,21]
  ]


-- used to expands the faces of a dodecahedron of edge length 2/gold into a rhombicosidodecahedron
magicTranslation :: RealFloat a => a
magicTranslation = 1.1755705


-- re use dodecahedron's points
-- take the 5 points of each face of the dodecahedron
-- put them into a single list (length 12 * 5)
-- translate to using the magic translation along the normal of their face
-- -> rhombicosidodecahedron vertice

stubRhombicosidodecahedronPoints :: RealFloat a => [Point3f a]
stubRhombicosidodecahedronPoints = rhombicosidodecahedronPointsFactory 0

rhombicosidodecahedronPoints :: RealFloat a => [Point3f a]
rhombicosidodecahedronPoints = rhombicosidodecahedronPointsFactory magicTranslation


rhombicosidodecahedronPointsFactory :: RealFloat a => a -> [Point3f a]
rhombicosidodecahedronPointsFactory expansionStep = foldr (++) [] $ map asList dodecahedronFaces
  where asList is = expandFace $ map (\i -> dodecahedronPoints !! i) is
        expandFace ps = map (add $ forceNorm expansionStep $ faceSum ps) ps
        faceSum ps = foldr add (Point3f 0 0 0) ps


rhombicosidodecahedronFaces :: [[Int]]
rhombicosidodecahedronFaces = rhombicosidodecahedronTriangles ++
                              rhombicosidodecahedronQuads ++
                              (chop 5 $ take 60 [0..])           -- pentagonal faces
  where toPoints (i,j,k) = [dodecahedronPoints !! i, dodecahedronPoints !! j, dodecahedronPoints !! k]


-- Snub Dodecahedron


-- used to expands the faces of a dodecahedron of edge length 2/gold into a rhombicosidodecahedron
magicSnubTranslation :: RealFloat a => a
magicSnubTranslation = 1.072163
-- used to rotate the pentagonal faces into a snub dodecahedron
magicSnubRotation :: RealFloat a => a
magicSnubRotation = 0.2287498


-- expand and rotate each face of the dodecahedron
snubDodecahedronPoints :: RealFloat a => [Point3f a]
snubDodecahedronPoints = foldr (++) [] $ map asList dodecahedronFaces
  where asList is = expandFace $ map (\i -> dodecahedronPoints !! i) is
        expandFace ps = map ((rotateFace ps ). (add $ times magicSnubTranslation $ faceNorm ps)) ps
        rotateFace ps = rotate magicSnubRotation $ faceNorm ps
        faceNorm ps = normalized $ foldr add (Point3f 0 0 0) ps


-- re use rhombicosidodecahedronFaces as much as possible, just split the quads intro triangles
snubDodecahedronFaces :: [[Int]]
snubDodecahedronFaces = rhombicosidodecahedronTriangles ++
                        (concatMap split rhombicosidodecahedronQuads) ++
                        (chop 5 $ take 60 [0..])           -- pentagonal faces
  where split [i,j,k,l] = [[i,j,k],[i,k,l]]


-- Pentagonal Hexecontahedron


-- dual of snub -> convert faces to vertice
pentagonalHexecontahedronPoints :: RealFloat a => [Point3f a]
pentagonalHexecontahedronPoints = map (center . toPoints) snubDodecahedronFaces
  where toPoints (i:is) = (snubDodecahedronPoints !! i) : toPoints is
        toPoints [] = []
        center ps = times ((/) 1 $ fromIntegral $ length ps) $ foldr add (Point3f 0 0 0) ps


snubFaceCount     = length snubDodecahedronFaces
snubPentagonCount = 12 -- per dodecahedron origin
snubTriangleCount = snubFaceCount - snubPentagonCount
snubPentagons     = drop snubTriangleCount snubDodecahedronFaces
snubTriangles     = take snubTriangleCount snubDodecahedronFaces


pentagonalHexecontahedronFaces :: [[Int]]
pentagonalHexecontahedronFaces = map toFaceIds stripsFromAllPentagons
  where toFaceIds l = map toFaceId l
        toFaceId indice = fromJust $ elemIndex indice snubDodecahedronFaces


stripsFromAllPentagons :: [[[Int]]]
stripsFromAllPentagons = concatMap stripsFromPentagon snubPentagons


stripsFromPentagon :: [Int] -> [[[Int]]]
stripsFromPentagon pentagonVerticeIds =
  map addPentagon $ map (findTriangleStrip snubTriangles (-1)) $ cyclicConsecutivePairs pentagonVerticeIds
  where addPentagon l = pentagonVerticeIds : l


findTriangleStrip :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
findTriangleStrip triangles notK (i, j) =
  if triangle == []
    then []
    else triangle : findTriangleStrip triangles i (k, j)
  where triangle = findTriangle i j notK triangles
        k = head $ dropWhile (\e -> e == i) $ dropWhile (\e -> e == j) triangle


findTriangle :: Int -> Int -> Int -> [[Int]] -> [Int]
findTriangle i j notK triangles = if result == [] then [] else head result
  where result = filter (notContains notK) $ filter (contains i) $ filter (contains j) triangles


-- icosahedron


icosahedronPoints :: RealFloat a => [Point3f a]
icosahedronPoints = [ Point3f 0 (1)  gold
                    , Point3f 0 (-1) gold
                    , Point3f 0 (-1) (-gold)
                    , Point3f 0 (1)  (-gold)
                    , Point3f (1)  (gold)  0
                    , Point3f (-1) (gold)  0
                    , Point3f (-1) (-gold) 0
                    , Point3f (1)  (-gold) 0
                    , Point3f (gold)  0 (1)
                    , Point3f (gold)  0 (-1)
                    , Point3f (-gold) 0 (-1)
                    , Point3f (-gold) 0 (1)
                    ]


-- faces are pointing inward
icosahedronFaces :: [[Int]]
icosahedronFaces = [ [0,1,8]
                   , [0,11,1]
                   , [2,3,9]
                   , [2,10,3]
                   , [4,5,0]
                   , [4,3,5]
                   , [6,7,1]
                   , [6,2,7]
                   , [8,9,4]
                   , [8,7,9]
                   , [10,11,5]
                   , [10,6,11]
                   , [0,8,4]
                   , [0,5,11]
                   , [1,7,8]
                   , [1,11,6]
                   , [2,9,7]
                   , [2,6,10]
                   , [3,4,9]
                   , [3,10,5]
                   ]


icosahedronRadius :: RealFloat a => a
icosahedronRadius = sqrt(gold * gold + 1)


-- floret tessellation of icosahedron


magicAngle :: RealFloat a => a
magicAngle = acos $ 3 / 14 * sqrt 21


magicScale :: RealFloat a => a
magicScale = 2 / 21 * sqrt 21


magicRotate :: RealFloat a => Normal a -> Point3f a -> Point3f a
magicRotate = rotate magicAngle


-- push the vertex of the floret solids onto their bounding sphere
floretPushing :: Bool
floretPushing = False


toFloretFace :: RealFloat a => [Point3f a] -> [[Point3f a]]
toFloretFace [p0, p1, p2] = assert (n1 == n2 && n2 == n0)
                                   [ [p0, p3, p9, p5, p8]
                                   , [p1, p4, p9, p3, p6]
                                   , [p2, p5, p9, p4, p7]
                                   ]
  where n0 = norm p0
        n1 = norm p1
        n2 = norm p2
        push = if floretPushing
          then forceNorm n0
          else id
        p9 = push $ (1/3) `times` (p0 `add` p1 `add` p2)
        transform = times magicScale . (magicRotate $ normalized p9)
        p3 = push $ p0 `add` (transform $ vec p0 p1)
        p4 = push $ p1 `add` (transform $ vec p1 p2)
        p5 = push $ p2 `add` (transform $ vec p2 p0)
        p6 = push $ p1 `add` (transform $ vec p1 p0)
        p7 = push $ p2 `add` (transform $ vec p2 p1)
        p8 = push $ p0 `add` (transform $ vec p0 p2)


thinFloretIco :: RealFloat a => [[Point3f a]]
thinFloretIco = concatMap ((map $ take 4) . toFloretFace . idsToFace) icosahedronFaces
  where idsToFace [i,j,k] = [icosahedronPoints !! i, icosahedronPoints !! j, icosahedronPoints !! k]


pentagonalFloretIco :: RealFloat a => [[Point3f a]]
pentagonalFloretIco = concatMap (toFloretFace . idsToFace) icosahedronFaces
  where idsToFace [i,j,k] = [icosahedronPoints !! i, icosahedronPoints !! j, icosahedronPoints !! k]


floretPoints :: RealFloat a => [[Point3f a]] -> [Point3f a]
floretPoints floretIco = concatMap id floretIco


floretFaces :: RealFloat a =>  [[Point3f a]] -> [[Int]]
floretFaces floretIco = replaceWithId 0 floretIco
  where replaceWithId _ [] = []
        replaceWithId n (face:faces) = take (length face) [n..] : (replaceWithId (n+length face) faces)


thinFloretPoints :: RealFloat a => [Point3f a]
thinFloretPoints = floretPoints thinFloretIco
thinFloretFaces = floretFaces thinFloretIco


pentaFloretPoints :: RealFloat a => [Point3f a]
pentaFloretPoints = floretPoints pentagonalFloretIco
pentaFloretFaces = floretFaces pentagonalFloretIco