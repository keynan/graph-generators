{-# LANGUAGE RecordWildCards #-}
module Data.Graph.Generators.Random.Clique(generateRandQClique) where

import Data.Graph.Generators
import Data.Graph.Generators.Clique(generateQClique)
import System.Random.MWC
import Control.Monad


{-|
  First construct a recusive clique 'tree' with parameters q and t.
  According to the random parameter `p`, remove edges from the graph.
  For p=1, retain the entire graph.
  As `p` approaches (decreases to) (1/q), then there is high probability the graph will become disconnected.
-}

generateRandQClique :: GenIO
                      -> Int
                      -> Int
                      -> Double
                      -> IO GraphInfo
generateRandQClique gen q t p = do
  let detG = generateQClique q t
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
