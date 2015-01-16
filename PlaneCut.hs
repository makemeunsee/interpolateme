{-# LANGUAGE RecordWildCards #-}

module PlaneCut ( FacedModel(FacedModel)
                , vertice, faces, normals
                , fromModel, toModel
                , cutModel
                , chain
                , Plane (..)
                , ToPlane (..)
                , intersectPlaneAndSegment
                )

where

--import System.IO.Unsafe (unsafePerformIO)

import Data.List (elemIndex, findIndex, findIndices, partition)
import Data.Array.IArray (IArray, Array, array, (!))

import qualified Geometry as G
import ListUtil


type Vertex a = ( G.Point3f a, [Int] )


data ToPlane = Above | Below | OnPlane deriving (Eq, Show)


data FacedModel a = FacedModel { vertice :: [ (Int, Vertex a ) ] -- geometric data + reference to faces a vertex belongs to
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
    m' = FacedModel (zip [0..] $ zip vs' $ G.facesForEachVertex m)
                    (zip fs [0..])
                    (zip (map (\f -> G.normalized $ foldr1 G.add $ map (ns !!) f) fs) [0..])


toModel :: RealFloat a => FacedModel a -> G.Model a
toModel FacedModel{..} = G.modelAutoNormals vs fs
  where
    vs = map (\(_,(p,_)) -> p) vertice
    fs = map (\(f,_) -> map (\i -> head $ findIndices (\(j,(_,_)) -> j == i) vertice) f) faces


-- list all edges of all faces in the model
edges :: FacedModel a -> [(Int, Int)]
edges FacedModel{..} = concatMap cyclicConsecutivePairs $ fst $ unzip faces


-- 3d plane, with normal (kx, ky, kz)
-- seed is a point belonging to the plane
data Plane f = Plane { kx :: f
                     , ky :: f
                     , kz :: f
                     , ptOfPl :: G.Point3f f }
               deriving (Eq, Show)


cutModel :: (RealFloat a, Show a) => a -> Plane a -> FacedModel a -> FacedModel a
cutModel tolerance plane@Plane{..} m@FacedModel{..} =
  if length newFace > 0 then
    FacedModel updatedVertice updatedFaces updatedNormals
  else
    m
  where
    -- first pass over the vertice, gathering data relative to the vertice and the truncation
    (cutArray, maxId) = rawCutData tolerance plane m
    cutState = (!) cutArray

    -- new vertice are appended, their indice start from an offset
    offset = 1 + maxId

    -- new face id = 1 + max old ids
    newFaceId = 1 + foldr (\(_,i) m -> if i > m then i else m) (-1) faces

    -- extract edges intersecting with the cutting plane
    toCut = cutEdges True cutArray (edges m)

    -- iterating over the list of edges to cut:
    -- find vertice created by the cut and assign them proper faces indice
    -- find vertice belonging to the new face
    -- create a dictionary for replacing cut edges with new edges
    (created, rawNewFaceVertice, _, cutEdgesIndex, cutEdgesValues) = foldr (\(i0, i1) (created, newFace, createdId, dictIds, dictVals) ->
        if elem (i0,i1) dictIds || elem (i1,i0) dictIds then -- if cut do not cut again
              (created, newFace, createdId, dictIds, dictVals)
        else
          let (_,(p0,fs0)) = head $ filter (\(i,(_,_)) -> i == i0) vertice in
          let (_,(p1,fs1)) = head $ filter (\(i,(_,_)) -> i == i1) vertice in
          let fs = newFaceId : intersection fs0 fs1 in
          case intersectPlaneAndSegment tolerance plane (p0,p1) of
            Nothing -> (created, newFace, createdId, dictIds, dictVals) -- should not happen
            Just p
              | cutState i0 == Above && cutState i1 == Below -> ( (createdId, (p, fs)) : created, ((p,fs), createdId) : newFace, createdId+1, (i0,i1) : dictIds, (createdId, i1) : dictVals)
              | cutState i1 == Above && cutState i0 == Below -> ( (createdId, (p, fs)) : created, ((p,fs), createdId) : newFace, createdId+1, (i0,i1) : dictIds, (i0, createdId) : dictVals)
              | cutState i0 == OnPlane -> (created, ((p0,newFaceId : fs0), i0) : newFace, createdId, dictIds, dictVals)
              | cutState i1 == OnPlane -> (created, ((p1,newFaceId : fs1), i1) : newFace, createdId, dictIds, dictVals)
              | otherwise -> (created, newFace, createdId, dictIds, dictVals)
      )
      ([], [], offset, [], [])
      toCut

    -- duplicates can occur in the extracted vertice of the new face
    newFaceVertice = removeDups rawNewFaceVertice

    -- chain vertice one after another, into the correct polygon
    chained = chain newFaceVertice []

    -- extract only the indice
    newFace = maybe [] (map (\((_,_),i) -> i) . order) chained

    -- replace cut segments, remove references to removed vertice
    updatedFaces0 = map (\(f,i) -> (cutFace f, i)) faces

    -- filter out removed faces
    (removedFaces0, updatedFaces1) = partition (\(f, _) -> f == []) updatedFaces0
    removedFaces = map (\(_,i) -> i) removedFaces0

    -- append the new face
    updatedFaces = (newFace, newFaceId) : updatedFaces1

    -- update neighbour info of vertice
    updatedVertice0 = map updateNeighbours $ filter (\(i,(_,_)) -> cutState i /= Above) vertice

    -- concat remaining vertice with created vertice
    updatedVertice = updatedVertice0 ++ created

    -- remove normals of removed faces
    updatedNormals0 = filter (\(_,i) -> not $ elem i removedFaces) normals

    -- add as many normals as needed, equal to the cutting plane normal
    planeNormal = G.Point3f kx ky kz
    updatedNormals = (planeNormal, newFaceId) : updatedNormals0

    -- add reference to the new face, remove dead references
    updateNeighbours (i, (p, ns)) =
     if cutState i == OnPlane
       then (i, (p, newFaceId : filter (\j -> not $ elem j removedFaces) ns))
       else (i, (p, ns))

    -- make a polygon face the same way as the cutting plane
    order is@(((p0,_),i0):((p1,_),i1):((p2,_),i2):_) =
      let normal = (G.vec p0 p1) `G.cross` (G.vec p1 p2) in
      let k = normal `G.dot` planeNormal in
      if k > 0
        then is
        else reverse is
    order is = is

    -- tranform into segments, replace cut segments, turnback into an indice list, remove dead references
    cutFace face = filter noDeadRef $ removeDups $ flattenSegments $ replaceSegments $ cyclicConsecutivePairs face

    -- filter out removed vertice references
    noDeadRef = \i -> i >= offset || cutState i /= Above

    -- using the cut edges dictionary, replace segments
    replaceSegments = map (\(i,j) -> case elemIndex (i,j) cutEdgesIndex of
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


rawCutData
  :: RealFloat a
  => a
  -> Plane a
  -> FacedModel a
  -> (Array Int ToPlane, Int)
rawCutData tolerance Plane{..} FacedModel{..} =
  (array (0, maxId) l, maxId)
  where
    (l, maxId) = foldr
      (\ v@(i, (pt, _)) (vs, maxId) -> case toPlane pt of
        x | abs x <= tolerance -> ( (i, OnPlane) : vs, max i maxId)
          | x < -tolerance     -> ( (i, Below) : vs, max i maxId)
          | otherwise          -> ( (i, Above) : vs, max i maxId)
      )
      ([],-1)
      vertice
    toPlane p = let (G.Point3f cx cy cz) = G.add p $ G.times (-1) ptOfPl in
                cx*kx+cy*ky+cz*kz


-- edges cut by the plane
cutEdges :: Bool -> Array Int ToPlane -> [(Int, Int)] -> [(Int, Int)]
cutEdges includeOn cutArray edges =
  filter (bothSides includeOn) edges
  where
    bothSides False (i,j) = cutPos i == Above && cutPos j == Below || cutPos j == Above && cutPos i == Below
    bothSides True (i,j) = cutPos i == OnPlane || cutPos j == OnPlane || bothSides False (i,j)
    cutPos = (!) cutArray


orderPoints p0@(G.Point3f x0 y0 z0) p1@(G.Point3f x1 y1 z1) =
  if x0 < x1 then
    (p0, p1)
  else if x1 < x0 then
    (p1, p0)
  else if y0 < y1 then
    (p0, p1)
  else if y1 < y0 then
    (p1, p0)
  else if z0 < z1 then
    (p0, p1)
  else if z1 < z0 then
    (p1, p0)
  else
    (p0, p1)


intersectPlaneAndSegment :: RealFloat a => a -> Plane a -> (G.Point3f a, G.Point3f a) -> Maybe (G.Point3f a)
intersectPlaneAndSegment tolerance Plane{..} (p0, p1) =
  if tolerance >= abs k then
    Nothing -- segment and plane parallel (or segment on plane)
  else
    Just $ G.add p0' $ G.times at v
  where (p0'@(G.Point3f x0 y0 z0), p1'@(G.Point3f x1 y1 z1)) = orderPoints p0 p1
        v@(G.Point3f dx dy dz) = G.add p1' $ G.times (-1) p0' -- p0p1 vector
        G.Point3f sx sy sz = ptOfPl
        k = kx*dx + ky*dy + kz*dz
        at = (kx*(sx-x0) + ky*(sy-y0) + kz*(sz-z0)) / k
        l = G.norm v
        position = at * l