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


depthMap :: Ord a => Labyrinth a -> (Map a [Int], Int, Int)
depthMap (Node i d ls) = foldr' (\(m', dmax, dmin) (m, oldMax, oldMin) ->
                                  (unionWith (++) m m', max dmax oldMax, min dmin oldMin))
                                (singleton i [d], d, d)
                                $ map depthMap ls


-- for each (value, depth) in the maze, gives the ids of the (value, depth) pairs which are the parent or a children.
neighboursMap :: Ord a => Maybe (a, Int) -> Labyrinth a -> Map (a, Int) [(a, Int)]
neighboursMap parent (Node i d ls) =
  let ns0 = if parent == Nothing then [] else [fromJust parent] in
  let ns = foldr (\(Node i' d' _) ns -> (i', d') : ns) ns0 ls in
  foldr' (\m' m ->
           unionWith (++) m m')
         (singleton (i, d) ns)
         $ map (neighboursMap (Just (i, d))) ls


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


-- Create a random maze, using the face neighbours as a topology.
-- The maze is an acyclic directed graph. Its nodes carry a value (identifier from the topology) and a depth.
-- The depth difference from a child node to its parent is always 1 or -1 (always 1 if alwaysDeeper is true).
-- maxBranchLength determines the maximum length from the root to any leaf this maze is allowed to grow to.
-- If maxBranchLength << the number of cell in the topology, the maze will cover only part of the topology.
-- minGapForOverlap determines the minimum depth gap between 2 nodes of the maze for them to carry the same identifier.
-- ie: If a branch of the maze is deep enough, it can use a cell from the topology previously used, but higher.
-- Having minGapForOverlap >= maxBranchLength is a guarantee to have no overlap (no branch will be become deep enough).
-- alwaysDeeper forces the maze branches to go always deeper.
-- If alwaysDeeper is False, each single continuous sequence of maze nodes will randomly go higher or deeper.
labyrinth1 :: RealFrac a => Seed -> Int -> Int -> Bool -> S.Seq (Face a) -> (Labyrinth Int, Seed)
labyrinth1 seed maxBranchLength minGapForOverlap alwaysDeeper topo
  | S.null topo = (Node (-1) (-1) [], seed) -- illegal
  | otherwise   = topologyToLabyrinth0 seed (singleton 0 [0]) [] [(0, 0, 0)] 1
  where
    topologyToLabyrinth0 :: Seed -> Map Int [Int] -> [(Int, Labyrinth Int)] -> [(Int, Int, Int)] -> Int -> (Labyrinth Int, Seed)
    topologyToLabyrinth0 seed visited acc [] dir = (snd $ head acc, seed)
    topologyToLabyrinth0 seed visited !acc ((i, depth, dist) : parents) dir =
      let explorable = filter (\(_, depths) ->
                                depths == [] || L.all (\d -> abs (depth - d) >= minGapForOverlap) depths
                              )
                              $ map (\n -> (n, maybe [] id $ Data.Map.lookup n visited))
                              $ neighbours $ S.index topo i in
      let l = length explorable in
      let (rndIndex, seed') = range_random (0, l) seed in
      if dist >= maxBranchLength || explorable == [] then
        let newAcc = case acc of
                       [] -> [(dist, Node i depth [])]
                       ls -> let (tails, parallelBranches) = L.partition (\(d, _) -> d == dist+1) ls in
                             if tails == [] then
                               (dist, Node i depth []) : ls
                             else
                               (dist, Node i depth $ snd $ unzip tails) : parallelBranches
                     in
        topologyToLabyrinth0 seed visited newAcc parents dir
      else
        let (newDir, seed'') = if alwaysDeeper || l == 1 then
                                 (dir, seed')
                               else
                                 let (rnd, seed'') = range_random (0,2) seed' in
                                 (2*rnd -1, seed')
                               in
        let (j, jDepths) = explorable !! rndIndex in
        let newVisited = insert j (depth : jDepths) visited in
        topologyToLabyrinth0 seed'' newVisited acc ((j, depth+dir,dist+1):(i, depth, dist):parents) newDir


faceIndice :: Integral a => a -> Face b -> [a]
faceIndice offset Face{..} =
  let l = length vertice in
  let centerIndex = 1 + 2 * fromIntegral l in
  let centerIndex' = 2 * fromIntegral l in
  let verticeIndice = take l [0..] in
  let idPairs = cyclicConsecutivePairs verticeIndice in
  concatMap (\(i,j) -> map (offset+) [centerIndex', 2*i+1, 2*j+1, centerIndex, 2*j, 2*i, 2*i, 2*j, 2*j+1, 2*i, 2*j+1, 2*i+1]) idPairs


depthFracValue :: RealFloat a => Int -> Int -> Int -> a
depthFracValue depthMin depthMax depth = fromIntegral (depth - depthMin) / depthSpan
  where depthSpan = fromIntegral $ depthMax - depthMin


toBufferData :: (RealFloat a, Integral b) => S.Seq (Face a) -> Map Int [Int] -> Int -> Int -> ([G.Point3f a], [b], [a], [a], [G.Point3f a], [a])
toBufferData faces depthMap depthMin depthMax = ( reverse vs
                                                , ids
                                                , reverse centers
                                                , reverse mazeData
                                                , reverse normals
                                                , reverse cellIds
                                                )
  where
    depth = depthFracValue depthMin depthMax
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
                               ( (concatMap (\v -> [G.times 0.98 v, v]) $ barycenter f : newVs) ++ vs'
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


toPathBufferData :: (RealFloat a, Integral b) => S.Seq (Face a) -> Int -> Int -> Map (Int, Int) [(Int, Int)] -> ([G.Point3f a], [b], [G.Point3f a], [a])
toPathBufferData faces depthMin depthMax neighboursMap = ( reverse vs
                                                         , ids
                                                         , reverse normals
                                                         , reverse mazeData
                                                         )
  where
    fracDepth = depthFracValue depthMin depthMax
    fNeighbours p = case Data.Map.lookup p neighboursMap of
                      Nothing -> []
                      Just ns -> ns

    junction f0 f1 =
      let [v0, v1] = intersection (vertice f0) (vertice f1) in
      G.times 0.5 $ G.add v0 v1

    (  vs
     , ids
     , normals
     , mazeData
     , _) = foldr' (\((i, d), neighbours) (vs, is, ns, md, offset) ->
                      let f = S.index faces i in
                      let bary = barycenter f in
                      let dv = fracDepth d in
                      foldr' (\(i', _) (vs', is', ns', md', offset') ->
                               let f' = S.index faces i' in
                               let j = junction f f' in
                               ( j : vs'
                               , offset : offset' : is'
                               , seed f : ns'
                               , dv : md'
                               , offset' + 1)
                             )
                             (bary : vs, is, seed f : ns, dv : md, offset + 1)
                             neighbours
                   )
                   ([], [], [], [], 0)
                   $ toAscList neighboursMap


toThickPathBufferData :: (RealFloat a, Integral b) => S.Seq (Face a) -> Int -> Int -> Map (Int, Int) [(Int, Int)] -> ([G.Point3f a], [b], [a], [G.Point3f a], [a])
toThickPathBufferData faces depthMin depthMax neighboursMap = ( reverse vs
                                                              , ids
                                                              , reverse centers
                                                              , reverse normals
                                                              , reverse mazeData
                                                              )
  where
    hFactor = 1 + 0.2 / log (fromIntegral (S.length faces))
    fracDepth = depthFracValue depthMin depthMax
    fNeighbours p = case Data.Map.lookup p neighboursMap of
                      Nothing -> []
                      Just ns -> ns

    junctions f0 f1 =
      let fVs0 = vertice f0 in
      let [v0, v1] = intersection fVs0 (vertice f1) in
      let d = G.add v1 $ G.times (-1) v0 in
      let (j0,j1) = (G.add v0 $ G.times 0.3 d, G.add v0 $ G.times 0.7 d) in
      ([j0,j1], [v0,v1])

    (  vs
     , ids
     , centers
     , normals
     , mazeData
     , _) = foldr' (\((i, d), neighbours) (vs, is, cs, ns, md, offset) ->
                      let f = S.index faces i in
                      let bary = barycenter f in
                      let dv = fracDepth d in
                      let fvs = vertice f in
                      case neighbours of
                        [(i',_)] ->
                          -- create a dead end
                          let f' = S.index faces i' in
                          let ([j0,j1], [v0,v1]) = junctions f f' in
                          let i0 = fromJust (L.elemIndex v0 fvs) in
                          let i1 = fromJust (L.elemIndex v1 fvs) in
                          let (oj0,oj1) = if i1 == i0+1 then
                                            (j0,j1)
                                          else if i0 == i1+1 then
                                            (j1,j0)
                                          else if i1 == 0 then
                                            (j0,j1)
                                          else
                                            (j1,j0)
                                          in
                          let d = G.add oj1 $ G.times (-1) oj0 in
                          let b0 = G.add bary $ G.times (-0.5) d in
                          let b1 = G.add bary $ G.times 0.5 d in
                          let c = G.times 0.25 $ oj0 `G.add` oj1 `G.add` b0 `G.add` b1 in
                          let newVs = [oj0,oj1,b1,b0] in
                          ( c : newVs ++ vs
                          , map (offset+) [ 4,1,0
                                          , 4,2,1
                                          , 4,3,2
                                          , 4,0,3] ++ is
                          , 1 : (take 4 $ repeat 0) ++ cs
                          , (take 5 $ repeat $ seed f) ++ ns
                          , (take 5 $ repeat dv) ++ md
                          , offset + 5)
                        _   ->
                          -- create a crossroad
                          let vsAndJs = foldr' (\(i', _) vjs ->
                                                 let f' = S.index faces i' in
                                                 let ([j0,j1], [v0, v1]) = junctions f f' in
                                                 insertBetween [j0,j1] v0 v1 vjs
                                               )
                                               fvs
                                               neighbours in
                          let js = filter (\v -> not $ L.elem v fvs) vsAndJs in
                          let l = length js in
                          let il = fromIntegral l in
                          let c = G.times (1 / fromIntegral l) $ foldr1 (\v acc -> G.add v acc) js in
                          let shiftedIds = map (offset+) $ take l [0..] in
                          let is' = (last shiftedIds) : (take (l-1) shiftedIds) in
                          ( c : js ++ vs
                          , (concatMap (\(i0,i1) -> [il+offset, i1, i0]) $ cyclicConsecutivePairs is') ++ is
                          , 1 : (take l $ repeat 0) ++ cs
                          , (take (l+1) $ repeat $ seed f) ++ ns
                          , (take (l+1) $ repeat dv) ++ md
                          , offset + il +1)
                   )
                   ([], [], [], [], [], 0)
                   $ toAscList neighboursMap
