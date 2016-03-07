{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import System.Environment
import System.Directory
import Data.Graph.Generators(GraphInfo(..))
import Data.Graph.Generators.Random.ErdosRenyi(erdosRenyiGraph)
import Data.Graph.Generators.Random.WattsStrogatz(wattsStrogatzGraph)
import Data.Graph.Generators.Random.BarabasiAlbert(barabasiAlbertGraph)
import Data.Graph.Generators.Clique(qCliqueGraph)
import Data.Graph.Generators.Random.Clique(nodeDeletion)
import System.Random.MWC(createSystemRandom)
import System.IO
import qualified Data.Map as Map
import Data.List (intersperse)

d  = 4
n  = 30
p  = toDbl d / toDbl n
b  = 0.28
m0 = 10

toDbl = fromInteger.toInteger

sampleDir = "dist/build/samples"

writeGraph :: FilePath -> GraphInfo -> IO ()
writeGraph path (GraphInfo{..}) =
  withFile path WriteMode
  (\handle -> do
      hPutStrLn handle "strict graph {"
      mapM (hPutStrLn handle) [ (show x) ++ " -- " ++ (show y)
                              | (x, y) <- edges ]
      hPutStrLn handle "}"
  )

tuples2Map :: Ord a => [(a, b)] -> Map.Map a [b]
tuples2Map = foldr g Map.empty
  where
    g (x, y) = Map.insertWith (++) x [y]

{-
main :: IO ()
main = do
  args <- getArgs
  gen <- createSystemRandom
  wG <- wattsStrogatzGraph gen n d b
  let mip = tuples2Map $ edges wG
  let mop = fmap ((Map.!) mip) [0..((Map.size mip)-1)]
  let mup = fmap ((intercalate " ") . (fmap show)) mop
  mapM_ (putStrLn) mup
  where
    intercalate x xs = concat (intersperse x xs)
-}


q = 5
t = 3

main :: IO ()
main = do
  args <- getArgs
  gen <- createSystemRandom
  eG <- erdosRenyiGraph gen n p
  wG <- wattsStrogatzGraph gen n d b
  bG <- barabasiAlbertGraph gen n d
  let qG = qCliqueGraph q t 
  qc <- nodeDeletion gen n qG
  createDirectoryIfMissing True "dist/build/samples"
  writeGraph (sampleDir ++ "/ErdosRenyiGraph.dot") eG
  writeGraph (sampleDir ++ "/WattsStrogatz.dot") wG
  writeGraph (sampleDir ++ "/BarabasiAlbertGraph.dot") bG
  writeGraph (sampleDir ++ "/qclique.dot") qc
  putStrLn "hello World."


