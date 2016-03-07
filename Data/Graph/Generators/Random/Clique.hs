{-# LANGUAGE RecordWildCards #-}
module Data.Graph.Generators.Random.Clique(
  nodeDeletion,
  qCliqueGraph) where

import Data.Graph.Generators
import qualified Data.Graph.Generators.Clique as C (qCliqueGraph)
import System.Random.MWC
import Control.Monad
import qualified Data.Set as Set
import Data.Set(Set)


{-|
  First construct a recusive clique 'tree' with parameters q and t.
  According to the random parameter `p`, remove edges from the graph.
  For p=1, retain the entire graph.
  As `p` approaches (decreases to) (1/q), then there is high probability the graph will become disconnected.
-}

qCliqueGraph :: GenIO
                      -> Int
                      -> Int
                      -> Double
                      -> IO GraphInfo
qCliqueGraph gen q t p = do
  let detG = C.qCliqueGraph q t
  let l = length $ edges detG
  rs <- f l gen
  let es = map (snd) $ filter ((<=p) . fst) $ zip rs $ edges detG
  return GraphInfo {
    edges = edges detG,
    numNodes = numNodes detG
    }
  where
    f 0 gen = return []
    f n gen = do
      x <- uniform gen :: IO Double
      xs <- f (n-1) gen
      return (x:xs)


-- delete random nodes and associated edges until there are n nodes left
nodeDeletion :: GenIO -> Int -> GraphInfo -> IO GraphInfo
nodeDeletion gen n GraphInfo{..} = do
  set <- f (==(numNodes-n)) Set.empty
  let es = filter (not . g set) edges
  return GraphInfo {
    edges = es,
    numNodes = n
    }
  where
    f :: (Int -> Bool) -> Set Int -> IO (Set Int)
    f p set
      | p (Set.size set) = return set
      | otherwise        = do
          x <- uniform gen :: IO Double
          let y = (floor $ x * (toDbl numNodes)) `mod` numNodes
          f p $ Set.insert y set
    g :: Set Int -> (Int, Int) -> Bool
    g set (x, y) = (x `Set.member` set) ||
                   (y `Set.member` set)
    toDbl = fromInteger . toInteger





