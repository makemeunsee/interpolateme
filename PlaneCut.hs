{-# LANGUAGE RecordWildCards #-}

module PlaneCut ( FacedModel(FacedModel)
                , vertice, faces, normals
                , fromModel, toModel
                , Plane(Plane)
                , cutModel
                , chain
                )

where

import Data.List (elem, elemIndex, findIndex, findIndices, partition)

import qualified Geometry as G
import ListUtil

type Vertex a = ( G.Point3f a, [Int], Int )

data FacedModel a = FacedModel { vertice :: [ Vertex a ] -- geometric data + reference to faces a vertex belongs to
                               , faces :: [ ( [ Int ], Int ) ]              -- list of references to vertice
                               , normals :: [ ( G.Point3f a, Int ) ]        -- 1 normal per face
                               }
                    deriving (Show, Eq)


fromModel :: RealFloat a => G.Model a -> FacedModel a
fromModel m@(G.Model vs fs ns) = m'
  where
    -- scale vertice, so that the distance from face centers to origin is 1
    center0 = G.faceBarycenter vs $ fs !! 0
    scale = map (G.divBy $ G.norm center0)
    vs' = scale vs
    m' = FacedModel (map (\(i,(p,f)) -> (p,f,i)) $ zip [0..] $ zip vs' $ G.facesForEachVertex m)
                    (zip fs [0..])
                    (zip (map (\f -> foldr1 G.add $ map (ns !!) f) fs) [0..])


toModel :: RealFloat a => FacedModel a -> G.Model a
toModel FacedModel{..} = G.modelAutoNormals vs fs
  where
    vs = map (\(p,_,_) -> p) vertice
    fs = map (\(f,_) -> map (\i -> head $ findIndices (\(_,_,j) -> j == i) vertice) f) faces


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
    -- first pass over the vertice, gathering data relative to the vertice and the truncation
    ( removedVerticeIds
     ,intactVerticeIds
     ,onPlaneVerticeIds
     ,keptVertice
     ,maxId) = rawCutData tolerance plane m

    -- new vertice are appended, their indice start from an offset
    offset = 1 + foldr (\(_,_,i) m -> if i > m then i else m) (-1) vertice

    -- new face id = 1 + max old ids
    newFaceId = 1 + foldr (\(_,i) m -> if i > m then i else m) (-1) faces

    -- extract edges intersecting with the cutting plane
    toCut = cutEdges True ( removedVerticeIds, intactVerticeIds, onPlaneVerticeIds ) (edges m)

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
            | elem i0 removedVerticeIds && elem i1 intactVerticeIds -> ((p, fs, createdId) : created, ((p,fs), createdId) : newFace, createdId+1, ((i0,i1),(createdId, i1)) : dict)
            | elem i0 intactVerticeIds && elem i1 removedVerticeIds -> ((p, fs, createdId) : created, ((p,fs), createdId) : newFace, createdId+1, ((i0,i1),(i0, createdId)) : dict)
            | elem i0 onPlaneVerticeIds -> (created, ((p0,newFaceId : fs0), i0) : newFace, createdId, dict)
            | elem i1 onPlaneVerticeIds -> (created, ((p1,newFaceId : fs1), i1) : newFace, createdId, dict)
            | otherwise -> (created, newFace, createdId, dict)
      )
      ([], [], offset, [])
      toCut

    -- a dictionary of cut edges, old values to new values
    (cutEdgesIndex, cutEdgesValues) = unzip cutEdgesMap

    -- duplicates can occur in the extracted vertice of the new face
    newFaceVertice = removeDups rawNewFaceVertice

    -- chain vertice one after another, into the correct polygon
    chained = chain newFaceVertice []

    -- extract only the indice
    newFace = maybe [] (map (\((_,_),i) -> i)) chained

    -- replace cut segments, remove references to removed vertice
    updatedFaces0 = map (\(f,i) -> (cutFace f, i)) faces

    -- filter out removed faces
    (removedFaces0, updatedFaces1) = partition (\(f, _) -> f == []) updatedFaces0
    removedFaces = map (\(_,i) -> i) removedFaces0

    -- append the new face
    updatedFaces = (order newFace, newFaceId) : updatedFaces1

    -- update neighbour info of vertice
    updatedVertice0 = map updateNeighbours keptVertice

    -- concat remaining vertice with created vertice
    updatedVertice = updatedVertice0 ++ created

    -- remove normals of removed faces
    updatedNormals0 = filter (\(_,i) -> not $ elem i removedFaces) normals

    -- add as many normals as needed, equal to the cutting plane normal
    planeNormal = G.Point3f kx ky kz
    updatedNormals = updatedNormals0 ++ [(planeNormal, newFaceId)]

    -- add reference to the new face, remove dead references
    updateNeighbours (p, ns, i) =
     if elem i onPlaneVerticeIds
       then (p, newFaceId : filter (\j -> not $ elem j removedFaces) ns, i)
       else (p, ns, i)

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

    -- tranform into segments, replace cut segments, turnback into an indice list, remove dead references
    cutFace face = filter noDeadRef $ removeDups $ flattenSegments $ replaceSegments $ cyclicConsecutivePairs face

    -- filter out removed vertice references
    noDeadRef = \i -> not $ elem i removedVerticeIds

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


rawCutData :: RealFloat a => a -> Plane a -> FacedModel a -> ( [Int]
                                                             , [Int]
                                                             , [Int]
                                                             , [Vertex a]
                                                             , Int)
rawCutData tolerance Plane{..} FacedModel{..} =
  foldl
    (\ (above, below, on, kept, maxId) v@(pt, _, i) -> case toPlane pt of
      x | abs x <= tolerance -> (above, below, i : on, v : kept, max i maxId)
        | x < -tolerance     -> (above, i : below, on, v : kept, max i maxId)
        | otherwise          -> (i : above, below, on, kept, max i maxId)
    )
    ([],[],[],[],-1)
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