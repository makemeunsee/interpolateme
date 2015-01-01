{-# LANGUAGE RecordWildCards #-}

module PlaneCut ( FacedModel(FacedModel)
                , vertice, faces, normals
                , Plane(Plane)
                , cutModel
                , chain
                )

where

import Data.List (elem, elemIndex, findIndex)

import qualified Geometry as G
import ListUtil

data FacedModel a = FacedModel { vertice :: [ ( G.Point3f a, [Int], Int ) ] -- geometric data + reference to faces a vertex belongs to
                               , faces :: [ ( [ Int ], Int ) ]              -- list of references to vertice
                               , normals :: [ ( G.Point3f a, Int ) ]        -- 1 normal per face
                               }
                    deriving (Show, Eq)


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
  if length newFace > 0 then
    FacedModel updatedVertice updatedFaces updatedNormals
  else
    m
  where
    -- check where each vertex stands to the cutting plane
    sortedVertice = split tolerance plane m
     -- gives us what vertice to keep as is, to remove, to be updated
    (removed, intact, onPlane) = sortedVertice

    -- first remove the cut vertice
    cleaned = filter (\(_, _, i) -> not $ elem i removed) vertice

    -- new vertice are appended, their indice start from an offset
    offset = 1 + foldr (\(_,_,i) m -> if i > m then i else m) (-1) vertice

    -- update the faces: update faces split in half, remove (and list) faces for which all vertice where removed
    (updatedFaces0, removedFaces, facesToCut) = updateFaces ([], [], []) faces

    -- remove normals of removed faces
    updatedNormals0 = filter (\(_,i) -> not $ elem i removedFaces) normals

    -- new face id = 1 + max old ids
    newFaceId = 1 + foldr (\(_,i) m -> if i > m then i else m) (-1) faces

    -- remove reference to removed faces, add reference to new face
    updateNeighbours (p, ns, i) =
     if elem i onPlane
       then (p, newFaceId : ns, i)
       else (p, ns, i)

    -- apply vertex indice update, removal of faces, split of faces
    updateFaces :: ([([Int], Int)],[Int],[Int]) -> [([Int], Int)] -> ([([Int], Int)],[Int],[Int])
    updateFaces acc [] = acc
    updateFaces (updated, removedIds, facesToCut) ((face,i):fs) =
      -- intact
      if all (\i -> elem i onPlane || elem i intact) face
        then updateFaces ((face, i) : updated, removedIds, facesToCut) fs -- keep the whole face
      -- fully removed
      else if all (\i -> elem i removed) face
        then updateFaces (updated, i : removedIds, facesToCut) fs
      -- cut
      else
        updateFaces ((face, i) : updated, removedIds, i : facesToCut) fs

    -- create the new face from cut edges and cut the relevant faces

    -- extract edges intersecting with the cutting plane
    toCut = cutEdges True sortedVertice (edges m)

    -- iterating over the list of edges to cut:
    -- find vertice created by the cut and assign them proper faces indice
    -- find vertice belonging to the new face
    -- create a dictionary for replacing cut edges with new edges
    (created, rawNewFaceVertice, _, cutEdgesMap) = foldr (\(i0, i1) (created, newFace, createdId, dict) ->
        let (p0,fs0,_) = head $ filter (\(_,_,i) -> i == i0) vertice in
        let (p1,fs1,_) = head $ filter (\(_,_,i) -> i == i1) vertice in
        let fs = newFaceId : intersection fs0 fs1 in
        case intersectPlaneAndSegment tolerance plane (p0,p1) of
          Nothing -> (created, newFace, createdId, dict) -- should not happen
          Just p
            | elem i0 removed && elem i1 intact -> ((p, fs, createdId) : created, ((p,fs), createdId) : newFace, createdId+1, ((i0,i1),(createdId, i1)) : dict)
            | elem i0 intact && elem i1 removed -> ((p, fs, createdId) : created, ((p,fs), createdId) : newFace, createdId+1, ((i0,i1),(i0, createdId)) : dict)
            | elem i0 onPlane -> (created, ((p0,newFaceId : fs0), i0) : newFace, createdId, dict)
            | elem i1 onPlane -> (created, ((p1,newFaceId : fs1), i1) : newFace, createdId, dict)
            | otherwise -> (created, newFace, createdId, dict)
      )
      ([], [], offset, [])
      toCut

    -- duplicates can occur in the extracted vertice of the new face
    newFaceVertice = removeDups rawNewFaceVertice

    -- chain vertice one after another, into the correct polygon
    chained = chain newFaceVertice []

    -- extract only the indice
    newFace = maybe [] (map (\((_,_),i) -> i)) chained

    -- update neighbour info of vertice, removing references to removed faces
    updatedVertice0 = map updateNeighbours cleaned

    -- concat remaining vertice with created vertice
    updatedVertice = updatedVertice0 ++ created
    -- add as many normals as needed, equal to the cutting plane normal
    planeNormal = G.Point3f kx ky kz
    updatedNormals = updatedNormals0 ++ [(planeNormal, newFaceId)]

    -- a dictionary of cut edges, old values to new values
    (cutEdgesIndex, cutEdgesValues) = unzip cutEdgesMap

    -- cut relevant faces
    updatedFaces1 = reverse updatedFaces0 ++ [(order newFace, newFaceId)]
    updatedFaces = map (\(f, i) ->
                       if elem i facesToCut then
                         (cutFace f, i)
                       else
                         (f, i)
                     )
                     updatedFaces1

--    newFaceSegments = cyclicConsecutivePairs newFace
--    updtFacesSegments = map (\(f,_) -> cyclicConsecutivePairs f) $ filter (\(_, i) -> elem i facesToCut) updatedFaces
--    coherent = all (\(i,j) -> any (\f -> elem (i,j) f || elem (j,i) f) updtFacesSegments) newFaceSegments

    -- make a polygon face the same way as the cutting plane
    order is@(i0:i1:i2:_) =
      let (p0,_,_) = head $ filter (\(_,_,i) -> i == i0) updatedVertice in
      let (p1,_,_) = head $ filter (\(_,_,i) -> i == i1) updatedVertice in
      let (p2,_,_) = head $ filter (\(_,_,i) -> i == i2) updatedVertice in
      let normal = (G.vec p0 p1) `G.cross` (G.vec p1 p2) in
      let k = normal `G.dot` planeNormal in
      if k > 0
        then is
        else reverse is
    order is = is

    -- tranform into segments, replace cut segments, turnback into an indice list, remove dead references, apply offset due to removed vertice
    cutFace face = filter noDeadRef $ removeDups $ flattenSegments $ replaceSegments $ cyclicConsecutivePairs face

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


-- chain created vertice into a polygon
chain [] acc = case acc of
  [] -> Nothing
  [_] -> Nothing
  (h:t) ->
    let ((_,hFs),_) = h in
    let ((_,lFs),_) = last t in
    if length (intersection hFs lFs) == 2 then
      Just acc
    else
      Nothing
chain (h:t) [] = chain t [h]
chain l acc@(((_, fs), _):ys) =
  case findIndex (\v@((_,fs'),_) -> (==) 2 $ length $ intersection fs fs') l of
    Just i -> chain (take i l ++ drop (i+1) l) $ (l !! i) : acc
    Nothing -> Nothing -- new face is inconsistent, cancel cut


-- split vertice into above, below and on plane
split :: RealFloat a => a -> Plane a -> FacedModel a -> ([Int], [Int], [Int])
split tolerance Plane{..} FacedModel{..} =
  foldl
    (\ (above, below, on) (pt, _, i) -> case toPlane pt of
      x | abs x <= tolerance -> (above, below, i : on)
        | x < -tolerance     -> (above, i : below, on)
        | otherwise          -> (i : above, below, on)
    )
    ([],[],[])
    vertice
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


intersectPlaneAndSegment :: RealFloat a => a -> Plane a -> (G.Point3f a, G.Point3f a) -> Maybe (G.Point3f a)
intersectPlaneAndSegment tolerance Plane{..} (p0@(G.Point3f x0 y0 z0), p1@(G.Point3f x1 y1 z1)) =
  if tolerance >= abs k then
    Nothing -- segment and plane parallel (or segment on plane)
  else
    Just $ G.add p0 $ G.times at v
  where v@(G.Point3f dx dy dz) = G.add p1 $ G.times (-1) p0 -- p0p1 vector
        G.Point3f sx sy sz = seed
        k = kx*dx + ky*dy + kz*dz
        at = (kx*(sx-x0) + ky*(sy-y0) + kz*(sz-z0)) / k
        l = G.norm v
        position = at * l