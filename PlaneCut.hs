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
    (updatedFaces0, removedFaces, facesToCut) = updateFaces ([], [], []) faces

    -- remove normals of removed faces
    updatedNormals0 = filter (\(_,i) -> not $ elem i removedFaces) normals

    -- new face id = 1 + max old ids
    newFaceId = 1 + foldr (\(_,i) m -> if i > m then i else m) (-1) faces

    -- new vertice are appended, their indice start from an offset
    offset = length updatedVertice0

    -- remove reference to removed faces, add reference to new face
    updateNeighbours ((p, ns), i) =
     if elem i toUpdate
       then ((p, newFaceId : ns), i)
       else ((p, ns), i)

    -- apply vertex indice update, removal of faces, split of faces
    updateFaces :: ([([Int], Int)],[Int],[Int]) -> [([Int], Int)] -> ([([Int], Int)],[Int],[Int])
    updateFaces acc [] = acc
    updateFaces (updated, removedIds, facesToCut) ((face,i):fs) =
      -- intact
      if all (\i -> elem i toUpdate || elem i intact) face
        then updateFaces ((applyRemovedVertexOffset face, i) : updated, removedIds, facesToCut) fs -- keep the whole face
      -- fully removed
      else if all (\i -> elem i removed) face
        then updateFaces (updated, i : removedIds, facesToCut) fs
      -- cut
      else
        updateFaces ((face, i) : updated, removedIds, i : facesToCut) fs

    -- some indice were removed, compute the new proper indice
    -- if indice 1 and 3 have been removed, old -> new indice correspondance would be:
    -- 0 -> 0, 1 -> *, 2 -> 1, 3 -> *, 4 -> 2, 5 -> 3, etc.
    applyRemovedVertexOffset = map (\i -> i - greaterThanCount removed i)
    greaterThanCount removedIds i = foldl (\count j -> if i > j then count+1 else count) 0 removedIds

    -- create the new face from cut edges and cut the relevant faces

    -- extract edges intersecting with the cutting plane
    toCut = cutEdges True sortedVertice (edges m)

    -- do the intersection
    afterCut = map (\(i,j) -> (edgeAfterCut tolerance plane vertice newFaceId (i,j), i, j)) toCut

    -- offset created vertice ids as "applyRemovedVertexOffset" is applied later to new vertice ids too
    newIdsTmpOffset = length removed

    -- extract newly created vertice and those which may have been updated along
    (created, rawNewFaceVertice, _, cutEdgesMap) = foldr (\((v0, v1), i, j) (created, newFace, createdId, dict) -> case (v0, v1) of
        (Just (i0, p0, fs0), Just (i1, p1, fs1))
          | i0 == -1 -> (((p0,fs0), createdId) : created, ((p0,fs0), createdId) : newFace, createdId+1, ((i,j),(createdId+newIdsTmpOffset, i1)) : dict)
          | i1 == -1 -> (((p1,fs1), createdId) : created, ((p1,fs1), createdId) : newFace, createdId+1, ((i,j),(i0, createdId+newIdsTmpOffset)) : dict)
          | elem newFaceId fs0 -> (created, ((p0, fs0), i0) : newFace, createdId, dict)
          | elem newFaceId fs1 -> (created, ((p1, fs1), i1) : newFace, createdId, dict)
          | otherwise -> (created, newFace, createdId, dict)
        (Just (k, p, fs), Nothing) -> (created, ((p,fs),k) : newFace, createdId, dict)
        (Nothing, Just (k, p, fs)) -> (created, ((p,fs),k) : newFace, createdId, dict)
        _ -> (created, newFace, createdId, dict)
      )
      ([], [], offset, [])
      afterCut

    -- duplicates can occur in the extracted vertice of the new face
    newFaceVertice = removeDups rawNewFaceVertice

    -- chain vertice one after another, into the correct polygon
    chained =  chain newFaceVertice []

    -- extract only the indice
    newFace = maybe [] (map (\((_,_),i) -> i)) chained

    -- update neighbour info of vertice, removing references to removed faces
    updatedVertice0 = fst $ unzip $ if length newFace > 0 then map updateNeighbours cleaned else cleaned

    -- concat remaining vertice with created vertice
    updatedVertice = updatedVertice0 ++ (reverse $ fst $ unzip created)
    -- add as many normals as needed, equal to the cutting plane normal
    planeNormal = G.Point3f kx ky kz
    updatedNormals = if length newFace > 0 then updatedNormals0 ++ [(planeNormal, newFaceId)] else updatedNormals0

    -- a dictionary of cut edges, old values to new values
    (cutEdgesIndex, cutEdgesValues) = unzip cutEdgesMap

    -- cut relevant faces
    updatedFaces1 = if length newFace > 0 then reverse updatedFaces0 ++ [(order newFace, newFaceId)] else reverse updatedFaces0
    updatedFaces = map (\(f, i) ->
                       if elem i facesToCut
                         then (cutFace f, i)
                         else (f, i)
                       )
                       updatedFaces1

    -- chain created vertice into a polygon
    chain [] acc = Just acc
    chain (h:t) [] = chain t [h]
    chain l acc@(((_, fs), _):ys) =
      case findIndex (\v@((_,fs'),_) -> (==) 2 $ length $ intersection fs fs') l of
        Just i -> chain (take i l ++ drop (i+1) l) $ (l !! i) : acc
        Nothing -> Nothing -- new face is inconsistent, cancel cut

    -- make a polygon face the same way as the cutting plane
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

    -- tranform into segments, replace cut segments, turnback into an indice list, remove dead references, apply offset due to removed vertice
    cutFace face = applyRemovedVertexOffset $ filter noDeadRef $ removeDups $ flattenSegments $ replaceSegments $ cyclicConsecutivePairs face

    -- filter out removed vertice references
    noDeadRef = \i -> not $ elem i removed

    -- using the cut edges dictionary, replace segments
    replaceSegments = map (\(i,j) -> case elemIndex (i, j) cutEdgesIndex of
                           Just k ->
                             cutEdgesValues !! k
                           Nothing -> case elemIndex (j, i) cutEdgesIndex of
                             Just k ->
                               let (i',j') = cutEdgesValues !! k in
                               (j',i')
                             Nothing ->
                               (i,j)
                         )

    flattenSegments = concatMap (\(i,j) -> [i,j])


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