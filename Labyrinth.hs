{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Labyrinth


where


import Random.MWC.Pure (Seed, range_random)
import Data.Map (Map, singleton, notMember, insert, (!), lookup, empty, unionWith, toAscList)
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Sequence as S
import Data.Foldable (foldr')
import Data.Maybe (fromJust, listToMaybe)

import qualified Geometry as G
import ListUtil
import VoronoiCut


-- a simple data structure for a graph / labyrinth.
-- each node/leaf should hold a unique index value
data Labyrinth a = Node a Int ![Labyrinth a]
                 deriving (Eq, Show)


-- the cell index held at the head of this labyrinth
nodeValue :: Labyrinth a -> a
nodeValue (Node i _ _) = i


nodeDepth :: Labyrinth a -> Int
nodeDepth (Node _ d _) = d


-- the number of elements in this labyrinth
size :: Labyrinth a -> Int
size (Node _ _ ls) = 1 + foldr' (\l s -> s + size l) 0 ls


longestBranch :: Labyrinth a -> Int
longestBranch (Node _ d ls) = foldr' (\l m -> max m $ longestBranch l) d ls


depthMap :: Ord a => Labyrinth a -> (Map a [Int], Int)
depthMap (Node i d ls) = foldr' (\(m', d') (m, oldMax) -> (unionWith (++) m m', max d' oldMax)) (singleton i [d], d) $ map depthMap ls


elem :: Eq a => a -> Labyrinth a -> Bool
elem k (Node j _ ls) = k == j || any (Labyrinth.elem k) ls


-- blindly insert a leaf of value 'k' into the labyrinth, at node of value 'at', if any
-- does NOT check for duplicates
insertAt :: Eq a => a -> a -> Labyrinth a -> Labyrinth a
insertAt k at laby = case laby of
  Node i d ls | i == at   -> Node i d $ (Node k (d+1) []) : ls       -- insert k as new leaf to found parent
              | otherwise -> Node i d $ map (insertAt k at) ls  -- explore the labyrinth to find the right parent


labyrinth2 :: S.Seq (Face a) -> Labyrinth Int
labyrinth2 topo
  | S.null topo = Node (-1) (-1) [] -- illegal
  | otherwise   = laby2Rec [0] (Node 0 0 []) (Set.singleton 0)
  where
    laby2Rec [] laby visited = laby
    laby2Rec stack laby visited = let currentId = head stack in
                                  let currentNeighbours = neighbours $ S.index topo currentId in
                                  case findExplorable currentId currentNeighbours visited of
                                    Just i        -> laby2Rec (i : stack) (insertAt i currentId laby) (Set.insert i visited)
                                    Nothing       -> laby2Rec (tail stack) laby visited
    findExplorable i ids visited = L.find (explorable i visited) ids
    -- a node (face) is explorable...
    explorable parentId visited i  =
      -- if it's not part of the maze already
      Set.notMember i visited &&
      -- and the faces it touches are either the parent face or not in the maze
      (all (\j -> j == parentId || Set.notMember j visited)
           $ neighbours $ S.index topo i)


-- create a random maze, using the face neighbours as a topology.
-- the maze graph has a maximum depth of maxDepth
-- a face can be part of the maze multiple times (overlapping), with a minimum depth gap of (minGapFrac * maxDepth / 100) between maze nodes referring to the same face.
-- if minGapFrac is set to 100, no overlap can happen.
labyrinth1 :: RealFrac a => Seed -> Int -> Int -> S.Seq (Face a) -> (Labyrinth Int, Seed)
labyrinth1 seed maxDepth minGapFrac topo
  | S.null topo = (Node (-1) (-1) [], seed) -- illegal
  | otherwise   = topologyToLabyrinth0 seed (singleton 0 [0]) [] [(0, 0)]
  where
    minGap = floor $ 0.01 * (fromIntegral $ maxDepth * minGapFrac)
    topologyToLabyrinth0 seed visited !acc ((i, depth) : parents) =
      let explorable = filter (\(_, depths) ->
                                depths == [] || L.all (\d -> abs (depth - d) > minGap) depths
                              )
                              $ map (\n -> (n, maybe [] id $ Data.Map.lookup n visited))
                              $ neighbours $ S.index topo i in
      let l = length explorable in
      let (rndIndex, seed') = range_random (0, l) seed in
      if depth >= maxDepth || explorable == [] then
        if depth > 0 then
          let newAcc = case acc of
                         [] -> [Node i depth []]
                         ls -> let (tails, parallelBranches) = L.partition (\l-> nodeDepth l == depth+1) ls in
                               if tails == [] then
                                 (Node i depth []) : ls
                               else
                                 (Node i depth tails) : parallelBranches
                       in
          topologyToLabyrinth0 seed visited newAcc parents
        else
          (Node i 0 acc, seed)
      else
        let (j, jDepths) = explorable !! rndIndex in
        let newVisited = insert j (depth : jDepths) visited in
        topologyToLabyrinth0 seed' newVisited acc ((j, depth+1):(i, depth):parents)


faceIndice :: Integral a => a -> Face b -> [a]
faceIndice offset Face{..} =
  let l = length vertice in
  let centerIndex = 1 + 2 * fromIntegral l in
  let centerIndex' = 2 * fromIntegral l in
  let verticeIndice = take l [0..] in
  let idPairs = cyclicConsecutivePairs verticeIndice in
  concatMap (\(i,j) -> map (offset+) [centerIndex', 2*i+1, 2*j+1, centerIndex, 2*j, 2*i, 2*i, 2*j, 2*j+1, 2*i, 2*j+1, 2*i+1]) idPairs


depthFracValue :: RealFloat a => Int -> Int -> a
depthFracValue maxDepth depth = fromIntegral depth / maxDepthF
  where maxDepthF = fromIntegral maxDepth


toBufferData :: (RealFloat a, Integral b) => S.Seq (Face a) -> Map Int [Int] -> Int -> ([G.Point3f a], [b], [a], [a], [G.Point3f a], [a])
toBufferData faces depthMap maxDepth = ( reverse vs
                                       , ids
                                       , reverse centers
                                       , reverse mazeData
                                       , reverse normals
                                       , reverse cellIds
                                       )
  where
    depth = depthFracValue maxDepth
    fc = fromIntegral $ S.length faces
    (  vs
     , ids
     , centers
     , normals
     , mazeData
     , cellIds
     , _) = foldr' (\(i, depths) (vs, is, cs, ns, md, fis, offset) ->
                      let f = S.index faces i in
                      let newVs = vertice f in
                      let l = length newVs in
                      foldr' (\d (vs', is', cs', ns', md', fis', offset') ->
                               let ms = take (2*l+2) $ repeat $ depth d in -- dToHalf pos
                               ( (concatMap (\v -> [G.times 0.975 v, v]) $ barycenter f : newVs) ++ vs'
                               , (faceIndice (fromIntegral offset') f) ++ is'
                               , 1 : 1 : (take (2*l) $ repeat 0) ++ cs'
                               , (take (2*l+2) $ repeat $ seed f) ++ ns'
                               , ms ++ md'
                               , (take (2*l+2) $ repeat $ fromIntegral i) ++ fis'
                               , offset' + 2*l + 2
                               )
                             )
                             (vs, is, cs, ns, md, fis, offset)
                             depths
                   )
                   ([], [], [], [], [], [], 0)
                   $ toAscList depthMap


toPathBufferData :: (RealFloat a, Integral b) => S.Seq (Face a) -> Map Int [Int] -> Int -> ([G.Point3f a], [b], [a])
toPathBufferData faces depthMap maxDepth = ( reverse vs
                                           , ids
                                           , reverse mazeData
                                           )
  where
    fracDepth = depthFracValue maxDepth
    fDepths faceId = fromJust $ Data.Map.lookup faceId depthMap

    junction f0 f1 =
      let [v0, v1] = intersection (vertice f0) (vertice f1) in
      G.times 0.5 $ G.add v0 v1

    (  vs
     , ids
     , mazeData
     , _) = foldr' (\(i, depths) (vs, is, md, offset) ->
                      let f = S.index faces i in
                      let bary = barycenter f in
                      foldr' (\d (vs', is', md', offset') ->
                               let actualNeighbours = map (\nId -> S.index faces nId) $ filter (L.elem (d-1) . fDepths) $ neighbours f in
                               let dv = fracDepth d in
                               foldr' (\f' (vs'', is'', md'', offset'') ->
                                        let j = junction f f' in
                                        let bary' = barycenter f' in
                                        ( bary : j : bary' : vs''
                                        , offset'' : offset''+1 : offset''+1 : offset''+2 : is''
                                        , dv : dv : dv : md''
                                        , offset'' + 3)
                               )
                               (vs', is', md', offset')
                               actualNeighbours
                             )
                             (vs, is, md, offset)
                             depths
                   )
                   ([], [], [], 0)
                   $ toAscList depthMap
