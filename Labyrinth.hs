{-# LANGUAGE RecordWildCards #-}
module Labyrinth

where

import qualified Data.List as L
import Data.Map (Map, singleton, notMember, insert, (!), lookup, empty, unionWith)
import qualified Data.Set as Set
import qualified Data.Sequence as S
import qualified Random.MWC.Pure as RND

import qualified Geometry as G
import ListUtil
import VoronoiCut

-- a simple data structure for a graph / labyrinth.
-- each node/leaf should hold a unique index value
data Labyrinth a = Node a Int ![Labyrinth a]
                 | Leaf a Int
                 deriving (Eq, Show)


-- the cell index held at the head of this labyrinth
value :: Labyrinth a -> a
value (Leaf i _) = i
value (Node i _ _) = i


values :: Labyrinth a -> [a]
values (Leaf i _) = [i]
values (Node i _ ls) = i : concatMap values ls


-- the number of elements in this labyrinth
size :: Labyrinth a -> Int
size (Leaf _ _) = 1
size (Node _ _ ls) = 1 + foldr (\l s -> s + size l) 0 ls


longestBranch :: Labyrinth a -> Int
longestBranch (Leaf _ d) = d+1 -- branch length = depth + 1
longestBranch (Node _ _ ls) = foldr (\l m -> max m $ longestBranch l) 0 ls


depthMap :: Ord a => Labyrinth a -> (Map a [Int], Int)
depthMap (Leaf i d) = (singleton i [d], d)
depthMap (Node i d ls) = foldr (\(m', d') (m, oldMax) -> (unionWith (++) m m', max d' oldMax)) (depthMap $ Leaf i d) $ map depthMap ls


elem :: Eq a => a -> Labyrinth a -> Bool
elem k (Leaf i _) = k == i
elem k (Node j _ ls) = k == j || any (Labyrinth.elem k) ls


-- blindly insert a leaf of value 'k' into the labyrinth, at node of value 'at', if any
-- does NOT check for duplicates
insertAt :: Eq a => a -> a -> Labyrinth a -> Labyrinth a
insertAt k at laby = case laby of
  Leaf i d | i == at   -> Node i d [Leaf k $ d+1]                 -- turn leaf into a parent node
           | otherwise -> Leaf i d                              -- return unconcerned leaf intact
  Node i d ls | i == at   -> Node i d $ (Leaf k $ d+1) : ls       -- insert k as new leaf to found parent
              | otherwise -> Node i d $ map (insertAt k at) ls  -- explore the labyrinth to find the right parent


labyrinth2 :: S.Seq (Face a) -> Labyrinth Int
labyrinth2 topo
  | S.null topo = Leaf (-1) 0 -- illegal
  | otherwise   = laby2Rec [0] (Leaf 0 0) (Set.singleton 0)
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


swapElts i j ls = [get k x | (k, x) <- zip [0..] ls]
  where get k x | k == i = ls !! j
                | k == j = ls !! i
                | otherwise = x


shuffle :: RND.Seed -> [a] -> ([a], RND.Seed)
shuffle seed xs = shuffle0 seed xs 0
  where
    shuffle0 s xs i =
      if i >= length xs -1 then
        (xs, s)
      else
        let (r,s') = RND.range_random (i, length xs -1) s in
        shuffle0 s' (swapElts i r xs) (i+1)


-- create random maze from the face connection graph, reaching all the faces.
-- minGapFrac allows overlapping of the maze: a cell can be part of the maze multiple times, if it's added at sufficiently distant depth in the maze.
-- minGapFrac is the minimum depth difference, as percentage of the face count, for which a cell can be re-entered.
-- for instance, for a solid with 200 faces, minGapFrac at 25, a depth diff of 50 is required to re-enter a cell.
-- set minGapFrac at 100 for no overlapping. negative values unsupported.
labyrinth1 :: RealFrac a => RND.Seed -> Int -> S.Seq (Face a) -> (Labyrinth Int, RND.Seed)
labyrinth1 seed minGapFrac topo
  | S.null topo = (Leaf (-1) 0, seed) -- illegal
  | otherwise   = fst $ topologyToLabyrinth0 seed (singleton 0 [0]) 0 0
  where
    maxDepth = S.length topo
    minGap = floor $ 0.01 * (fromIntegral $ maxDepth * minGapFrac)
    topologyToLabyrinth0 seed visited i depth =
      let (shuffled, seed0) = shuffle seed $ neighbours $ S.index topo i in
      let (subnodes, seed', visited') = foldr (\j (newNodes, oldSeed, oldVisited) ->
                                                case Data.Map.lookup j oldVisited of
                                                  Just l | L.any (\d -> abs (depth - d) <= minGap) l ->
                                                           (newNodes, oldSeed, oldVisited)
                                                         | otherwise ->
                                                           let ((newNode, newSeed), newVisited) = topologyToLabyrinth0 oldSeed (insert j (depth : l) oldVisited) j $ depth+1 in
                                                           (newNode : newNodes, newSeed, newVisited)
                                                  _ ->
                                                    let ((newNode, newSeed), newVisited) = topologyToLabyrinth0 oldSeed (insert j [depth] oldVisited) j $ depth+1 in
                                                    (newNode : newNodes, newSeed, newVisited)
                                              )
                                              ([], seed0, visited)
                                              shuffled in
      if depth < maxDepth && subnodes /= [] then
        ((Node i depth subnodes, seed'), visited')
      else
        ((Leaf i depth, seed0), visited)

-- visit all paths once
--labyrinth1 :: S.Seq (Face a) -> Labyrinth In
--labyrinth1 seed topo
--  | S.null topo = Leaf (-1) 0 -- illegal
--  | otherwise   = fst $ topologyToLabyrinth0 Set.empty 0 0
--  where
--    maxDepth = S.length topo
--    topologyToLabyrinth0 visited i depth =
--      let (subnodes, visited') = foldr (\j (newNodes, oldVisited) ->
--                                                case Set.member (j,i) oldVisited of
--                                                  True -> (newNodes, oldVisited)
--                                                  False ->
--                                                    let (newNode, newVisited) = topologyToLabyrinth0 (Set.insert (j,i) $ Set.insert (i,j) oldVisited) j $ depth+1 in
--                                                    (newNode : newNodes, newVisited)
--                                              )
--                                              ([], visited)
--                                              $ neighbours $ S.index topo i in
--      if subnodes /= [] then
--        (Node i depth subnodes, visited')
--      else
--        (Leaf i depth, visited)


-- given a solid (vertice and faces) and a labyrinth which values are face indice,
-- build the 3D vertex data representing this labyrinth
-- labyrinth MUST be consistent with solid topology
labyrinthToPathVertice :: RealFloat a => S.Seq (Face a) -> Labyrinth Int -> Int -> ([G.Point3f a], [a])
labyrinthToPathVertice faces (Leaf i d) maxDepth = labyrinthToPathVertice faces (Node i d []) maxDepth
labyrinthToPathVertice faces (Node i d ls) maxDepth = ( bary : (concatMap (uncurry prependBaryCenter) subnodesVerts')
                                                      , (depth d) : concatMap ((depth d) :) subnodesDepths
                                                      )
  where
    depth = depthValue maxDepth
    face = S.index faces i
    bary = barycenter face
    (subnodesVerts, subnodesDepths) = unzip $ map (\l -> labyrinthToPathVertice faces l maxDepth) ls
    subnodesVerts' = zip subnodesVerts $ map value ls
    prependBaryCenter vs j = (junctionPoint j) : vs
    junctionPoint j =
      let [v0, v1] = intersection (vertice face) (vertice $ S.index faces j) in
      G.times 0.5 $ G.add v0 v1


-- build a list of indice defining segments, to be used along labyrinthToVertice
labyrinthToPathIndice :: Integral a => a -> Labyrinth b -> [a]
labyrinthToPathIndice offset (Leaf _ _) = []
labyrinthToPathIndice offset (Node _ _ ls) = indice
  where
    (_, indice) = foldl (\(o,ids) l -> (o+(fromIntegral $ 2*size l), offset : o+1 : o+1 : o+2 : labyrinthToPathIndice (o+2) l ++ ids)) (offset, []) ls


labyrinthToWallVertice :: RealFloat a => S.Seq (Face a) -> Labyrinth Int -> [(G.Point3f a, G.Point3f a)] -> [G.Point3f a]
labyrinthToWallVertice faces (Leaf i d) parentEdges = labyrinthToWallVertice faces (Node i d []) parentEdges
labyrinthToWallVertice faces (Node i _ ls) parentEdges = ownWalls ++ concatMap (\n -> labyrinthToWallVertice faces n edges) ls
  where
    edges = cyclicConsecutivePairs $ vertice $ S.index faces i
    childEdges = concatMap (\l -> cyclicConsecutivePairs $ vertice $ S.index faces $ value l) ls
    toAvoid = parentEdges ++ childEdges
    ownWalls = concatMap (\(i,j) -> [i,j]) $ filter (\(j,k) -> not $ L.elem (k,j) toAvoid) edges


labyrinthToWallIndice :: Integral a => a -> [[Int]] -> Labyrinth Int -> ([a], a)
labyrinthToWallIndice offset faces (Leaf d x) = labyrinthToWallIndice offset faces (Node x d [])
labyrinthToWallIndice offset faces (Node i _ ls) = (indice, newOffset)
  where
    wallCount = length (faces !! i) - length ls
    newOffset0 = offset + (fromIntegral wallCount) * 2
    ownIndice = concatMap (\j -> map (j*2 + offset + ) [0,1]) $ take wallCount [0..]
    (indice, newOffset) = foldl (\(ids, o) l ->
                                  let (newIds, o') = labyrinthToWallIndice o faces l in
                                  (ids ++ newIds, o')
                                )
                                (ownIndice, newOffset0)
                                ls


faceIndice :: Integral a => a -> Face b -> [a]
faceIndice offset Face{..} =
  let l = length vertice in
  let centerIndex = 1 + 2 * fromIntegral l in
  let centerIndex' = 2 * fromIntegral l in
  let verticeIndice = take l [0..] in
  let idPairs = cyclicConsecutivePairs verticeIndice in
  concatMap (\(i,j) -> map (offset+) [centerIndex', 2*i+1, 2*j+1, centerIndex, 2*j, 2*i, 2*i, 2*j, 2*j+1, 2*i, 2*j+1, 2*i+1]) idPairs


depthValue :: RealFloat a => Int -> Int -> a
depthValue maxDepth depth = (1 + maxDepthF - fromIntegral depth) / maxDepthF
  where maxDepthF = fromIntegral maxDepth


toBufferData :: (RealFloat a, Integral b) => S.Seq (Face a) -> [(Int, [Int])] -> Int -> ([G.Point3f a], [b], [a], [a], [G.Point3f a])
toBufferData faces depthMap maxDepth = (reverse vs, ids, reverse centers, reverse mazeData, reverse normals)
  where
    depth = depthValue maxDepth
    fc = fromIntegral $ S.length faces
    (  vs
     , ids
     , centers
     , normals
     , mazeData
     , _) = foldr (\(i, depths) (vs, is, cs, ns, md, offset) ->
                      let f = S.index faces i in
                      let newVs = vertice f in
                      let l = length newVs in
                      case depths of
                        [] ->
                          let ms = take ((+) 2 $ (*) 2 $ length $ vertice f) $ repeat 0 in
                          ( (concatMap (\v -> [G.times 0.975 v, v]) $ barycenter f : newVs) ++ vs
                          , (faceIndice (fromIntegral offset) f) ++ is
                          , 1 : 1 : (take (2*l) $ repeat 0) ++ cs
                          , (take (2*l+2) $ repeat $ seed f) ++ ns
                          , ms ++ md
                          , offset + 2*l + 2)
                        _ -> foldr (\d (vs', is', cs', ns', md', offset') ->
                                     let ms = take ((+) 2 $ (*) 2 $ length $ vertice f) $ repeat $ depth d in -- dToHalf pos
                                     ( (concatMap (\v -> [G.times 0.975 v, v]) $ barycenter f : newVs) ++ vs'
                                     , (faceIndice (fromIntegral offset') f) ++ is'
                                     , 1 : 1 : (take (2*l) $ repeat 0) ++ cs'
                                     , (take (2*l+2) $ repeat $ seed f) ++ ns'
                                     , ms ++ md'
                                     , offset' + 2*l + 2)
                                   )
                                   (vs, is, cs, ns, md, offset)
                                   depths
                  )
                  ([], [], [], [], [], 0)
                  depthMap