{-# LANGUAGE RecordWildCards #-}

module PlaneCut

where

import Data.List (elem, elemIndex, findIndex)

import System.IO.Unsafe (unsafePerformIO)

import qualified Geometry as G
import ListUtil

data FacedModel a = FacedModel { vertice :: [ ( G.Point3f a, [Int] ) ] -- geometric data + reference to faces a vertex belongs to
                               , faces :: [ ( [ Int ], Int ) ]                  -- list of references to vertice
                               , normals :: [ ( G.Point3f a, Int ) ]            -- 1 normal per face
                               }
                    deriving Show


-- list unique edges in the model
edges :: FacedModel a -> [(Int, Int)]
edges FacedModel{..} = foldr (\pairs acc ->
                         (filter (\(i,j) ->
                           not $ elem (i,j) acc || elem (j,i) acc
                         ) pairs) ++ acc
                       )
                       []
                       $ map cyclicConsecutivePairs $ fst $ unzip faces


-- 3d plane, with normal (kx, ky, kz)
-- seed is a point belonging to the plane
data Plane f = Plane { kx :: f
                     , ky :: f
                     , kz :: f
                     , seed :: G.Point3f f }
               deriving (Eq, Show)


--tolerance = 0.00001


data PlaneSide = Above | Below | On

positionToPlane :: RealFloat a => a -> Plane a -> G.Point3f a -> PlaneSide
positionToPlane tolerance Plane{..} p =
  let (G.Point3f cx cy cz) = G.add p $ G.times (-1) $ seed in
  let k = cx*kx+cy*ky+cz*kz in
  if abs k <= tolerance then On
  else if k < 0 then Below
  else Above


cutModel :: (Show a, RealFloat a) => a -> Plane a -> FacedModel a -> FacedModel a
cutModel tolerance plane@Plane{..} m@FacedModel{..} =
  FacedModel updatedVertice updatedFaces updatedNormals
  where
    -- generate index of vertice
    indexed = zip vertice [0..]

    -- check where each vertex stands to the cutting plane
    sortedVertice = split tolerance plane m
     -- gives us what vertice to keep as is, to remove, to be updated
    (removed, intact, toUpdate) = sortedVertice

    -- first remove the cut vertice
    cleaned = filter (\(v,i) -> not $ elem i removed) indexed

    -- update the faces: update faces split in half, remove (and list) faces for which all vertice where removed
    (updatedFaces0, removedFaces) = updateFaces ([], []) faces

    -- remove normals of removed faces
    updatedNormals0 = filter (\(_,i) -> not $ elem i removedFaces) normals

    -- new face id = 1 + max old ids
    newFaceId = 1 + foldr (\(_,i) m -> if i > m then i else m) (-1) faces

    -- update neighbour info of vertice, removing references to removed faces
    updatedVertice0 = fst $ unzip $ map updateNeighbours cleaned

    -- new vertice are appended, their indice start from an offset
    offset = length updatedVertice0

    -- remove reference to removed faces, add reference to new face
    updateNeighbours ((p, ns), i) =
     if elem i toUpdate
       then ((p, newFaceId : ns), i)
       else ((p, ns), i)

    -- apply vertex indice update, removal of faces, split of faces
    updateFaces acc [] = acc
    updateFaces (updated, removedIds) ((face,i):fs) =
      -- intact
      if all (\i -> elem i toUpdate || elem i intact) face
        then updateFaces ((applyRemovedVertexOffset face, i) : updated, removedIds) fs -- keep the whole face
      -- fully removed
      else if all (\i -> elem i removed) face
        then updateFaces (updated, i : removedIds) fs
      -- cut
      else
        updateFaces (([], i) : updated, removedIds) fs -- TODO handle cut faces

    applyRemovedVertexOffset = map (\i -> i - greaterThanCount removed i)
    greaterThanCount removedIds i = foldl (\count j -> if i > j then count+1 else count) 0 removedIds

    -- create the new face from cut edges

    -- extract edges intersecting with the cutting plane
    toCut = cutEdges True sortedVertice (edges m)

    -- do the intersection
    afterCut = map (edgeAfterCut tolerance plane vertice newFaceId) toCut

    -- extract newly created vertice and those which may have been updated along
    (created, rawNewFaceVertice, _) = foldr (\(v0, v1) (created, newFace, createdId) -> case (v0, v1) of
        (Just (-1, p0, fs0), Just v) -> (((p0,fs0), createdId) : created, ((p0,fs0), createdId) : newFace, createdId+1)
        (Just v, Just (-1, p1, fs1)) -> (((p1,fs1), createdId) : created, ((p1,fs1), createdId) : newFace, createdId+1)
        (Just (i0, p0, fs0), Just (i1, p1, fs1)) -> (created, (if elem newFaceId fs0 then ((p0, fs0), i0) else ((p1, fs1), i1)) : newFace, createdId)
        (Just (i, p, fs), Nothing) -> (created, ((p,fs),i) : newFace, createdId)
        (Nothing, Just (i, p, fs)) -> (created, ((p,fs),i) : newFace, createdId)
        _ -> (created, newFace, createdId)
      )
      ([], [], offset)
      afterCut

    newFaceVertice = removeDups rawNewFaceVertice

    -- concat remaining vertice with created vertice
    updatedVertice = updatedVertice0 ++ (reverse $ fst $ unzip created)
    -- add as many normals as needed, equal to the cutting plane normal
    planeNormal = G.Point3f kx ky kz
    updatedNormals = if length newFaceVertice > 0 then updatedNormals0 ++ [(planeNormal, newFaceId)] else updatedNormals0

    -- chain created vertice into a polygon
    chain [] acc = acc
    chain (h:t) [] = chain t [h]
    chain l acc@(((_, fs), _):ys) =
      let Just i = findIndex (\v@((_,fs'),_) -> (==) 2 $ length $ intersection fs fs') l in
      chain (take i l ++ drop (i+1) l) $ (l !! i) : acc

    chained = unsafePerformIO $ do
      putStrLn $ show newFaceVertice
      putStrLn $ show removedFaces
      putStrLn $ show $ length vertice
      putStrLn $ show $ length updatedVertice0
      putStrLn $ show $ length updatedVertice
      putStrLn $ show $ length normals
      putStrLn $ show $ length updatedNormals0
      putStrLn $ show $ length updatedNormals
      putStr "newFaceVertice: "
      putStrLn $ show newFaceVertice
      putStrLn ""
      let r = chain newFaceVertice []
      putStr "chained: "
      putStrLn $ show r
      putStrLn ""
      putStr "all verts ("
      putStr $ show $ length updatedVertice
      putStr ") ("
      putStr $ show $ length updatedNormals
      putStr "): "
      putStrLn $ show updatedVertice
      putStrLn ""
      return r

    newFace = map (\((_,_),i) -> i) chained
    order is@(i0:i1:i2:_) =
      let (p0,_) = updatedVertice !! i0 in
      let (p1,_) = updatedVertice !! i1 in
      let (p2,_) = updatedVertice !! i2 in
      let normal = (G.vec p0 p1) `G.cross` (G.vec p1 p2) in
      let k = normal `G.dot` planeNormal in
      if k > 0
        then is
        else reverse is
    order is = is
    updatedFaces = if length newFace > 0 then reverse updatedFaces0 ++ [(order newFace, newFaceId)] else reverse updatedFaces0

--    cleanFace = filter (\i -> not $ elem (i-offset) removed)
--
--    (cutEdgesMap, _) = foldr (\((a,b),(c,d)) (acc, o) -> case (c,d) of
--                          (Just (-1,_,_), Just (i,_,_)) -> (((a+offset,b+offset), [o,i+offset]):acc, o+1)
--                          (Just (i,_,_), Just (-1,_,_)) -> (((a+offset,b+offset), [i+offset,o]):acc, o+1)
--                          (Just (i,_,_), Just (j,_,_)) -> (((a+offset,b+offset), [i+offset,j+offset]):acc, o)
--                          (Just (i,_,_), Nothing) -> (((a+offset,b+offset), [i+offset]):acc, o)
--                          (Nothing, Just (i,_,_)) -> (((a+offset,b+offset), [i+offset]):acc, o)
--                          _ -> (((a+offset,b+offset), []) : acc, o)
--                        )
--                        ([], 0)
--                        $ zip toCut afterCut
--    (cutEdgesKeys, cutEdgesValues) = unzip cutEdgesMap
--
--    replaceCuts face =
--      let pairs = cyclicConsecutivePairs face in
--      let replaced = map (\(i,j) -> case elemIndex (i,j) cutEdgesKeys of
--                           Just i -> cutEdgesValues !! i
--                           Nothing -> case elemIndex (j,i) cutEdgesKeys of
--                             Just i -> reverse $ cutEdgesValues !! i
--                             Nothing -> [i,j]
--                         ) pairs in
--      let flat = concatMap id replaced in
--      collapse $ if head flat == last flat then tail flat else flat
--      where
--        collapse [] = []
--        collapse [a] = [a]
--        collapse (x0:x1:xs) = if x0 == x1 then collapse (x1:xs) else x0 : collapse (x1:xs)


-- split vertice into above, below and on plane
split :: RealFloat a => a -> Plane a -> FacedModel a -> ([Int], [Int], [Int])
split tolerance Plane{..} FacedModel{..} =
  foldl
    (\ (above, below, on) ((pt, _), i) -> case toPlane pt of
      x | abs x <= tolerance -> (above, below, i : on)
        | x < -tolerance     -> (above, i : below, on)
        | otherwise          -> (i : above, below, on)
    )
    ([],[],[])
    $ zip vertice [0..]
  where
    toPlane p = let (G.Point3f cx cy cz) = G.add p $ G.times (-1) seed in
                cx*kx+cy*ky+cz*kz


-- edges cut by the plane
cutEdges :: Bool -> ([Int], [Int], [Int]) -> [(Int, Int)] -> [(Int, Int)]
cutEdges includeOn (above, below, on) edges =
  filter (bothSides includeOn) edges
  where
    bothSides False (i,j) = elem i above == elem j below
    bothSides True (i,j) = elem i on || elem j on || bothSides False (i,j)


-- from split vertice, find split faces
-- returns ids of (intact faces, removed faces, split faces)
splitFaces :: ([Int], [Int]) -> [[Int]] -> ([Int], [Int], [Int])
splitFaces (vAbove, vBelowOrOn) faces =
  foldl
    (\ (intact, removed, cut) (face, i) ->
      if all (\i -> elem i vBelowOrOn) face then (i : intact, removed, cut)
      else if all (\i -> elem i vAbove) face then (intact, i : removed, cut)
      else (intact, removed, i : cut)
    )
    ([],[],[])
    $ zip faces [0..]
  where


data Intersection a = OnPoint (G.Point3f a)
                    | OnSegment (G.Point3f a)
                    | None
                    deriving (Show, Eq)


intersectPlaneAndSegment :: RealFloat a => a -> Plane a -> (G.Point3f a, G.Point3f a) -> Intersection a
intersectPlaneAndSegment tolerance Plane{..} (p0@(G.Point3f x0 y0 z0), p1@(G.Point3f x1 y1 z1)) =
  if tolerance >= abs k
    then None -- segment and plane parallel (or segment on plane)
    else
      if abs position <= tolerance then OnPoint p0
      else if abs (position - l) <= tolerance then OnPoint p1
      else if at > 0 && at < 1 then OnSegment $ G.add p0 $ G.times at v
      else None
  where v@(G.Point3f dx dy dz) = G.add p1 $ G.times (-1) p0 -- p0p1 vector
        G.Point3f sx sy sz = seed
        k = kx*dx + ky*dy + kz*dz
        at = (kx*(sx-x0) + ky*(sy-y0) + kz*(sz-z0)) / k
        l = G.norm v
        position = at * l


edgeAfterCut :: RealFloat a => a -> Plane a -> [(G.Point3f a, [Int])] -> Int -> (Int, Int) -> (Maybe (Int, G.Point3f a, [Int]), Maybe (Int, G.Point3f a, [Int]))
edgeAfterCut tolerance plane@Plane{..} vertice newFaceId (i0, i1) =
  let (p0, fs0) = vertice !! i0 in
  let (p1, fs1) = vertice !! i1 in
  let intersect = intersectPlaneAndSegment tolerance plane (p0, p1) in
  case intersect of
    None -> (Just (i0, p0, fs0), Just (i1, p1, fs1))
    OnPoint p
      | p == p0 && underPlane p1 -> (Just (i0, p0, newFaceId : fs0), Just (i1, p1, fs1))
      | p == p0 -> (Just (i0, p0, newFaceId : fs0), Nothing)
      | p == p1 && underPlane p0 -> (Just (i0, p0, fs0), Just (i1, p1, newFaceId : fs1))
      | p == p1 -> (Nothing, Just (i1, p1, newFaceId : fs1))
    OnSegment p
      | underPlane p0 -> (Just (i0, p0, fs0), Just (-1, p, newFaceId : intersection fs0 fs1))
      | underPlane p1 -> (Just (-1, p, newFaceId : intersection fs0 fs1), Just (i1, p1, fs1))
  where underPlane v = let (G.Point3f cx cy cz) = G.add v $ G.times (-1) $ seed in
                       cx*kx+cy*ky+cz*kz < 0