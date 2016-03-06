module Data.Graph.Generators.Clique(
  expectedEdgeCount
  ,generateQClique
  ) where

import Data.Graph.Generators
import Data.Graph.Generators.Regular(completeGraph)

import qualified Data.Set as Set
import Data.Set(Set)

import qualified Data.Map as Map
import Data.Map (Map)

type C a = Set a

insert :: Ord a => a -> C a -> C a
insert = (Set.insert)

replace :: (Ord a) => a -> a -> C a -> C a
replace x y c = y `insert` (Set.filter (/=x) c)

cons :: Ord a => a -> C a
cons x = Set.singleton x

nil :: Ord a => C a
nil = Set.empty

merge :: Ord a => C a -> C a -> C a
merge = Set.union

fold :: Ord a => (a -> b -> b) -> b -> C a -> b
fold = Set.foldr

fromL :: Ord a => [a] -> C a
fromL = Set.fromList

toL :: Ord a => C a -> [a]
toL = Set.toList

mapC :: Ord b => (a->b) -> C a -> C b
mapC = Set.map

{-|

exported largely for testing purposes.

-}

expectedEdgeCount :: Integral a => a -> a -> a
expectedEdgeCount q t = ((q*(q-1)) `div` 2) + ((q+1)^t) - 1

{-|

  Deterministically generate a recursive clique tree.
  (see: Recursive graphs with small-world scale-free properties by Comellas et al. http://arxiv.org/abs/cond-mat/0402033)

  
-}

generateQClique :: Int {-^ q, the size of each clique -}
                -> Int {-^ t, the number of recursions / depth of the tree -}
                -> GraphInfo
generateQClique q t = GraphInfo {
  edges = fst $ g t,
  numNodes = (((q+1)^t)  -1) `div` q + q
  }
  where
    g :: Int -> ([(Int, Int)], (Int, C (C Int)))
    g 0 = let es = edges $ completeGraph q in
           (es, (q, cons $ fromL [0..(q-1)]))
    g t = let (es, (nc, cliques)) = g (t-1) in
          let (nc', ees, cqs) =
                fold (h) (nc, [], nil) cliques in
          (es ++ ees, (nc', cliques `merge` cqs))
      where
        h :: C Int
          -> (Int, [(Int, Int)], C (C Int))
          -> (Int, [(Int, Int)], C (C Int))
        h c (i, es, s) =
          let f r = replace r i c in
          let lqc = mapC (f) c in
          let les = map (\x -> (i, x)) $ toL c in
          (i+1, es ++ les, merge s lqc)




