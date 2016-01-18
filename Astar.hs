
module Astar
(
Fringe,
Node(..),
Cost,
fringeAdd,
popFirstNode,
pickFirstNode,
selectFromFringe,
extractNextNodes,
search,
searchGoal,
gsearchGoal)
where
  import GHC.Float
  import Debug.Trace
  import Data.PSQueue as PQ

  type Cost = Int
  data Node state = Node state Cost Cost String ([Node state]) deriving (Show, Eq, Ord)
  type Fringe state = PQ.PSQ (Node state) Cost

  pathcost :: Node state -> Cost
  pathcost (Node _ acost _ _ []) = acost
  pathcost (Node _ acost _ _ (parent:parents)) = acost + (pathcost parent)

  fcost :: Node state -> Cost
  fcost (Node state acost hcost desc parent) = (pathcost (Node state acost hcost desc parent)) + hcost
  
  fringeAdd :: Ord state => Node state -> Fringe state -> Fringe state
  fringeAdd x f = insert x (fcost x) f

  popFirstNode :: Ord state => Fringe state -> Fringe state
  popFirstNode f = deleteMin f
  
  unwrapNode :: Maybe (PQ.Binding (Node state) Cost) -> [Node state]
  unwrapNode Nothing = []
  unwrapNode (Just x) = [PQ.key x]

  pickFirstNode :: Ord state => Fringe state -> [Node state]
  pickFirstNode f = unwrapNode (findMin f)

  selectFromFringe :: Ord state => Fringe state -> ([Node state], Fringe state)
  selectFromFringe a = (pickFirstNode a, popFirstNode a)

  fringeSize :: Fringe state -> Int
  fringeSize x = size x

  extractNextNodes :: Node state -> (state -> [(Node state, Cost)]) -> [Node state]
  extractNextNodes (Node s gcost hcost desc parent) f = [(Node s1 acost hcost1 desc1 ([Node s gcost hcost desc parent])) | ((Node s1 c1 hcost1 desc1 p1), acost) <- f s]

  fringeExtend :: Ord state => [Node state] -> Fringe state -> Fringe state
  fringeExtend [] f = f
  fringeExtend (x:xs) f = fringeExtend xs (fringeAdd x f)

  appendHashedFringe :: Ord state => Node state -> Fringe state -> [String] -> (state -> String) -> (Fringe state, [String])
  appendHashedFringe (Node x ac hc desc p) f hashes hash_func 
    | ((hash_func x) `elem` hashes) == True = (f, hashes)
    | otherwise = (fringeAdd (Node x ac hc desc p) f, hashes++[hash_func(x)])
      
  extendHashedFringe :: Ord state => [Node state] -> Fringe state -> [String] -> (state -> String) -> (Fringe state, [String])
  extendHashedFringe [] f hashes hash_func = (f, hashes)
  extendHashedFringe (x:xs) f hashes hash_func = extendHashedFringe xs new_fringe new_hashes hash_func where
        (new_fringe, new_hashes) = appendHashedFringe x f hashes hash_func
  
  maxNonInfiniteFloat :: RealFloat a => a -> a
  maxNonInfiniteFloat a = encodeFloat m n where
    b = floatRadix a
    e = floatDigits a
    (_, e') = floatRange a
    m = b ^ e - 1
    n = e' - e

  nodeCost :: Node state -> Cost
  nodeCost (Node s cost ecost desc history) = ecost

  search :: (Show state, Ord state) => Fringe state -> (state -> [(Node state, Cost)]) -> [Node state]
  search f extractor 
    | PQ.null f == True = []
    | nodeCost(head (pickFirstNode f)) == 0 = pickFirstNode f
    | otherwise = search (fringeExtend (extractNextNodes (head (pickFirstNode f)) extractor) (popFirstNode f)) extractor

  searchGoal :: (Show state, Ord state) => state -> (state -> Cost) -> (state -> [(Node state, Cost)]) -> [Node state] 
  searchGoal state evaluator extractor = search (PQ.singleton (Node state 0 (evaluator state) "Start point" []) (evaluator state)) extractor

--  filterNodes :: Show state => [(Node state, Float)] -> [String] -> ([(Node state, Cost)], [String])
--  filterNodes [] a = ([], a)
--  filterNodes x  a = (filteredList, new_hash) where
--        filteredList = [((Node state g h hist), f) | ((Node state g h hist), f)]

  gsearch :: (Show state, Ord state) => Fringe state -> (state -> [(Node state, Cost)]) -> [String] -> (state -> String) -> [Node state]
  gsearch x extractor hashes hash_func
    | PQ.null(x) = []
    | (nodeCost(head (pickFirstNode x)) == 0) = pickFirstNode x
    | otherwise = gsearch new_fringe extractor new_hash hash_func where
         (new_fringe, new_hash) = extendHashedFringe (extractNextNodes (head (pickFirstNode x)) extractor) (popFirstNode x) hashes hash_func

  gsearchGoal :: (Show state, Ord state) => state -> (state -> Cost) -> (state -> [(Node state, Cost)]) -> (state -> String) -> [Node state]
  gsearchGoal state evaluator extractor hash_func = gsearch (PQ.singleton (Node state 0 (evaluator state) "Start point" []) (evaluator state)) extractor [] hash_func


