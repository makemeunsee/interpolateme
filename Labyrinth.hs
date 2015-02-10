{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Labyrinth


where


import Random.MWC.Pure (Seed, range_random)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Sequence as S
import Data.Foldable (foldr')
import Data.Maybe (fromJust, isNothing, isJust, fromMaybe, fromJust)

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


depthMap :: Ord a => Labyrinth a -> (M.Map a [Int], Int, Int)
depthMap (Node i d ls) = foldr' (\(m', dmax, dmin) (m, oldMax, oldMin) ->
                                  (M.unionWith (++) m m', max dmax oldMax, min dmin oldMin))
                                (M.singleton i [d], d, d)
                                $ map depthMap ls


-- for each (value, depth) in the maze, gives the ids of the (value, depth) pairs which are the parent or a children.
neighboursMap :: Ord a => Maybe (a, Int) -> Labyrinth a -> M.Map (a, Int) [(a, Int)]
neighboursMap parent (Node i d ls) =
  let ns0 = if isNothing parent then [] else [fromJust parent] in
  let ns = foldr (\(Node i' d' _) ns -> (i', d') : ns) ns0 ls in
  foldr' (M.unionWith (++))
         (M.singleton (i, d) ns)
         $ map (neighboursMap (Just (i, d))) ls


elem :: Eq a => a -> Labyrinth a -> Bool
elem k (Node j _ ls) = k == j || any (Labyrinth.elem k) ls


-- blindly insert a leaf of value 'k' into the labyrinth, at node of value 'at', if any
-- does NOT check for duplicates
insertAt :: Eq a => a -> a -> Labyrinth a -> Labyrinth a
insertAt k at laby = case laby of
  Node i d ls | i == at   -> Node i d $ Node k (d+1) [] : ls       -- insert k as new leaf to found parent
              | otherwise -> Node i d $ map (insertAt k at) ls  -- explore the labyrinth to find the right parent


-- return the nth element of a map (last if n >= size, first if n <= 0)
getNth :: M.Map Int Int -> Int -> (Int, Int)
getNth m n
  | n <= 0 = min
  | n >= M.size m = max
  | n < M.size bott = getNth bott n
  | isJust lookup && M.size bott == n = (mid, fromJust lookup)
  | isJust lookup = getNth top (n - M.size bott - 1)
  | otherwise = getNth top (n - M.size bott)
    where max@(iMax, kMax) = M.findMax m
          min@(iMin, kMin) = M.findMin m
          mid = (iMax - iMin) `div` 2 + iMin
          (bott, lookup, top) = (M.splitLookup mid m)


buildMazeFromLists :: Int -> (Int, S.Seq [Int]) -> Labyrinth Int
buildMazeFromLists k (d, lists) = let kids = lists `S.index` k in
                                  Node k d $ map (`buildMazeFromLists` (d+1, lists)) kids


-- simple, breadth first maze
breadthFirstMaze :: Seed -> S.Seq (Face a) -> (Labyrinth Int, Seed)
breadthFirstMaze seed faces = (buildMazeFromLists first (0, result), seed'')
  where
    l = S.length faces
    (first, seed') = range_random (0,l) seed
    (result, seed'') = breadthFirst seed'
                                    (Set.singleton first)
                                    (M.fromList $ map (\n -> (n, first)) $ neighbours $ S.index faces first)
                                    (S.fromList $ replicate l [])
    breadthFirst :: Seed -> Set.Set Int -> M.Map Int Int -> S.Seq [Int] -> (S.Seq [Int], Seed)
    breadthFirst s visited nexts acc
      | M.null nexts = (acc, s)
      | otherwise = let (n, s') = range_random (0, M.size nexts) s in
                    let (fid, parent) = nexts `getNth` n in
                    if fid `Set.member` visited then
                      breadthFirst s'
                                   visited
                                   (M.delete fid nexts)
                                   acc
                    else
                      breadthFirst s'
                                   (Set.insert fid visited)
                                   (foldr (\n m -> M.insert n fid m) (M.delete fid nexts) $ neighbours $ S.index faces fid)
                                   (S.adjust (fid:) parent acc)


-- depth first maze (backtracking)
depthFirstMaze :: RealFrac a => Seed -> S.Seq (Face a) -> (Labyrinth Int, Seed)
depthFirstMaze seed topo
  | S.null topo = (Node (-1) (-1) [], seed) -- illegal
  | otherwise   =
    let (id0, seed0) = range_random (0, S.length topo) seed in
    topologyToLabyrinth0 seed0 (M.singleton id0 [0]) [] [(id0, 0)]
  where
    topologyToLabyrinth0 :: Seed -> M.Map Int [Int] -> [Labyrinth Int] -> [(Int, Int)] -> (Labyrinth Int, Seed)
    topologyToLabyrinth0 seed _ acc [] = (head acc, seed)
    topologyToLabyrinth0 seed visited !acc ((i, depth) : parents) =
      -- find explorable cells around current position
      let explorable = filter (null . snd)
                              $ map (\n -> (n, fromMaybe [] $ M.lookup n visited))
                              $ neighbours $ S.index topo i in
      if null explorable then
        -- nothing explorable, backtrack
        let newAcc = case acc of
                       -- simple dead end
                       [] -> [Node i depth []]
                       -- along the backtrack
                       -- are there branches to combine with at this point?
                       ls -> let (tails, parallelBranches) = L.partition (\l -> nodeDepth l == depth+1) ls in
                             if null tails then
                               -- no branch to combine with, create a new one
                               Node i depth [] : ls
                             else
                               -- merge tailing branches into one new branch
                               Node i depth tails : parallelBranches
                     in
        -- backtrack 1 step
        topologyToLabyrinth0 seed visited newAcc parents
      else
        -- exploring further
        let l = length explorable in
        -- pick a random cell as next step
        let (rndIndex, seed') = range_random (0, l) seed in
        let (j, jDepths) = explorable !! rndIndex in
        let newVisited = M.insert j (depth : jDepths) visited in
        -- go deeper
        topologyToLabyrinth0 seed' newVisited acc ((j, depth+1):(i, depth):parents)


-- Wilson's maze algorithm: uniform spanning tree over the topology of faces.
-- (actually not sure the uniformity property holds in this implementation but at least it's a spanning tree, built using a loop erased random walk)
wilsonMaze :: Seed -> S.Seq (Face a) -> (Labyrinth Int, Seed)
wilsonMaze seed faces = (buildMazeFromLists first (0, result), seed'')
  where
    l = S.length faces
    (first, seed') = range_random (0,l) seed
    points = Set.fromList $ take l [0..]
    (result, seed'') = buildPaths seed' (Set.delete first points) (Set.singleton first) (S.fromList $ replicate l [])
    growPath s path = let ns = neighbours $ S.index faces $ head path in
                      let (k, s') = range_random (0, length ns) s in
                      let n = ns !! k in
                      case L.elemIndex n path of
                        Nothing -> (n:path, s')
                        Just i  -> (drop i path, s')
    recGrowPath s visited path = let (newPath, s') = growPath s path in
                                 if head newPath `Set.member` visited then
                                   (newPath, s')
                                 else
                                   recGrowPath s' visited newPath
    updateLists lists (x0:x1:xs) = S.adjust (x1:) x0 $ updateLists lists (x1:xs)
    updateLists lists _ = lists
    buildPaths s pts visited acc = if Set.null pts then
                                     (acc, s)
                                   else
                                     let pt = Set.findMin pts in
                                     let (path, s') = recGrowPath s visited [pt] in
                                     let (newVisited, newPts) = foldr (\i (vs, ps) -> (Set.insert i vs, Set.delete i ps))
                                                                      (visited, Set.delete pt pts)
                                                                      path in
                                     let newAcc = updateLists acc path in
                                     buildPaths s' newPts newVisited newAcc


-- Create a depth first, random maze using the face neighbours as a topology.
-- The maze is an acyclic directed graph. Its nodes carry a value (identifier from the topology) and a depth.
-- The depth difference from a child node to its parent is always 1 or -1 (always 1 if alwaysDeeper is true).
-- maxBranchLength determines the maximum length from the root to any leaf this maze is allowed to grow to.
-- If maxBranchLength << the number of cell in the topology, the maze will cover only part of the topology.
-- minGapForOverlap determines the minimum depth gap between 2 nodes of the maze for them to carry the same identifier.
-- ie: If a branch of the maze is deep enough, it can use a cell from the topology previously used, but higher.
-- Having minGapForOverlap >= maxBranchLength is a guarantee to have no overlap (no branch will be become deep enough).
-- alwaysDeeper forces the maze branches to go always deeper.
-- If alwaysDeeper is False, each single continuous sequence of maze nodes will randomly go higher or deeper.
depthFirstMaze' :: RealFrac a => Seed -> Int -> Int -> Bool -> S.Seq (Face a) -> (Labyrinth Int, Seed)
depthFirstMaze' seed maxBranchLength minGapForOverlap alwaysDeeper topo
  | S.null topo = (Node (-1) (-1) [], seed) -- illegal
  | otherwise   =
    let (id0, seed0) = range_random (0, S.length topo) seed in
    topologyToLabyrinth0 seed0 (M.singleton id0 [0]) [] [(id0, 0, 0)] 1
  where
    topologyToLabyrinth0 :: Seed -> M.Map Int [Int] -> [(Int, Labyrinth Int)] -> [(Int, Int, Int)] -> Int -> (Labyrinth Int, Seed)
    topologyToLabyrinth0 seed _ acc [] _ = (snd $ head acc, seed)
    topologyToLabyrinth0 seed visited !acc ((i, depth, dist) : parents) dir =
      let explorable = filter (\(_, depths) ->
                                null depths || L.all (\d -> abs (depth - d) >= minGapForOverlap) depths
                              )
                              $ map (\n -> (n, fromMaybe [] $ M.lookup n visited))
                              $ neighbours $ S.index topo i in
      let l = length explorable in
      let (rndIndex, seed') = range_random (0, l) seed in
      if dist >= maxBranchLength || null explorable then
        let newAcc = case acc of
                       [] -> [(dist, Node i depth [])]
                       ls -> let (tails, parallelBranches) = L.partition (\(d, _) -> d == dist+1) ls in
                             if null tails then
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
                                 (2*rnd -1, seed'')
                               in
        let (j, jDepths) = explorable !! rndIndex in
        let newVisited = M.insert j (depth : jDepths) visited in
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


toBufferData :: (RealFloat a, Integral b) => S.Seq (Face a) -> M.Map Int [Int] -> Int -> Int -> ([G.Point3f a], [b], [a], [a], [G.Point3f a], [a])
toBufferData faces depthMap depthMin depthMax = ( reverse vs
                                                , ids
                                                , reverse centers
                                                , reverse mazeData
                                                , reverse normals
                                                , reverse cellIds
                                                )
  where
    depth = depthFracValue depthMin depthMax
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
                               let ms = replicate (2*l+2) (depth d) in -- dToHalf pos
                               ( concatMap (\v -> [G.times 0.98 v, v]) (barycenter f : newVs) ++ vs'
                               , faceIndice (fromIntegral offset') f ++ is'
                               , 1 : 1 : replicate (2*l) 0 ++ cs'
                               , replicate (2*l+2) (seed f) ++ ns'
                               , ms ++ md'
                               , replicate (2*l+2) (fromIntegral i) ++ fis'
                               , offset' + 2*l + 2
                               )
                             )
                             (vs, is, cs, ns, md, fis, offset)
                             depths
                   )
                   ([], [], [], [], [], [], 0)
                   $ M.toAscList depthMap


toPathBufferData :: (RealFloat a, Integral b) => S.Seq (Face a) -> Int -> Int -> M.Map (Int, Int) [(Int, Int)] -> ([G.Point3f a], [b], [G.Point3f a], [a])
toPathBufferData faces depthMin depthMax neighboursMap = ( reverse vs
                                                         , ids
                                                         , reverse normals
                                                         , reverse mazeData
                                                         )
  where
    fracDepth = depthFracValue depthMin depthMax

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
                   $ M.toAscList neighboursMap


toThickPathBufferData :: (RealFloat a, Integral b) => S.Seq (Face a) -> Int -> Int -> M.Map (Int, Int) [(Int, Int)] -> ([G.Point3f a], [b], [a], [G.Point3f a], [a])
toThickPathBufferData faces depthMin depthMax neighboursMap = ( reverse vs
                                                              , ids
                                                              , reverse centers
                                                              , reverse normals
                                                              , reverse mazeData
                                                              )
  where
    -- hFactor = 1 + 0.2 / log (fromIntegral (S.length faces))
    fracDepth = depthFracValue depthMin depthMax

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
                          let (oj0,oj1) | i1 == i0+1 = (j0,j1)
                                        | i0 == i1+1 = (j1,j0)
                                        | i1 == 0 = (j0,j1)
                                        | otherwise = (j1,j0)
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
                          , 1 : replicate 4 0 ++ cs
                          , replicate 5 (seed f) ++ ns
                          , replicate 5 dv ++ md
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
                          let js = filter (`L.notElem` fvs) vsAndJs in
                          let l = length js in
                          let il = fromIntegral l in
                          let c = G.times (1 / fromIntegral l) $ foldr1 G.add js in
                          let shiftedIds = map (offset+) $ take l [0..] in
                          let is' = last shiftedIds : take (l-1) shiftedIds in
                          ( c : js ++ vs
                          , concatMap (\(i0,i1) -> [il+offset, i1, i0]) (cyclicConsecutivePairs is') ++ is
                          , 1 : replicate l 0 ++ cs
                          , replicate (l+1) (seed f) ++ ns
                          , replicate (l+1) dv ++ md
                          , offset + il +1)
                   )
                   ([], [], [], [], [], 0)
                   $ M.toAscList neighboursMap
