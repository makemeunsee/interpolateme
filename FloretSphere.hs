module FloretSphere ( tetrahedron
                    , cube
                    , dodecahedron
                    , icosahedron
                    , polyhedrons
                    )
where

import Control.Exception (assert)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import ListUtil
import Geometry
import Geometry (Point3f, Normal, modelAutoNormals)


-- some polyhedrons created along the way...
tetrahedron :: RealFloat a => Model a
tetrahedron = modelAutoNormals tetrahedronPoints tetrahedronFaces

cube :: RealFloat a => Model a
cube = modelAutoNormals cubePoints cubeFaces

dodecahedron :: RealFloat a => Model a
dodecahedron = modelAutoNormals dodecahedronPoints dodecahedronFaces

icosahedron :: RealFloat a => Model a
icosahedron = modelAutoNormals icosahedronPoints icosahedronFaces

polyhedrons :: RealFloat a => [Model a]
polyhedrons = [ tetrahedron
              , cube
              , dodecahedron
              , icosahedron
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
            , [0,1,7,6]
            , [2,3,5,4]
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
icosahedronFaces = [ [0,1,8]  -- 0
                   , [0,11,1] -- 1
                   , [2,3,9]  -- 2
                   , [2,10,3] -- 3
                   , [4,5,0]  -- 4
                   , [4,3,5]  -- 5
                   , [6,7,1]  -- 6
                   , [6,2,7]  -- 7
                   , [8,9,4]  -- 8
                   , [8,7,9]  -- 9
                   , [10,11,5]-- 10
                   , [10,6,11]-- 11
                   , [0,8,4]  -- 12
                   , [0,5,11] -- 13
                   , [1,7,8]  -- 14
                   , [1,11,6] -- 15
                   , [2,9,7]  -- 16
                   , [2,6,10] -- 17
                   , [3,4,9]  -- 18
                   , [3,10,5] -- 19
                   ]


icosahedronRadius :: RealFloat a => a
icosahedronRadius = sqrt(gold * gold + 1)