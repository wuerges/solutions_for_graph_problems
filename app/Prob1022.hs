module Main where

import System.IO
import Control.Applicative
import Control.Monad
import Data.Tuple

import qualified Data.IntMap as M
import qualified Data.Set as S


type GMap = M.IntMap (S.Set Int)
newtype G = G GMap
    deriving Show

-- | Reverses a Map used by the Graph
reverseGMap :: GMap -- ^ The Map to reverse
            -> GMap
reverseGMap = M.fromListWith S.union . concatMap revT . M.toList
    where -- | Flips the tuple, generating many new tuples
          revT :: (Int, S.Set Int) -> [(Int, S.Set Int)]
          revT (k, vs) = (k, S.empty) : zip (S.toList vs) (repeat $ S.singleton k)

-- | Reverses all the edges of a Graph
reverseG :: G -> G
reverseG (G m) = G (reverseGMap m)

-- | Decomposes a Map and key into a into a tuple
-- with its values and the map without the key
decomp :: Int -> GMap -> Maybe ([Int], GMap)
decomp k m = do
    vs <- M.lookup k m
    return (S.toList vs, M.delete k m)

-- | Performs a BFS on the nodes of a graph
bfs :: G     -- ^ The Graph
    -> [Int] -- ^ The starting nodes
    -> [Int]
bfs (G m) []     = []
bfs (G m) (k:ks) = case decomp k m of
    Just (cs, m') -> k:bfs (G m') (ks ++ cs)
    Nothing       -> bfs (G m) ks

-- | Looks for the sink vertices from a graph
sinks (G m) = filter (\k -> S.null $ m M.! k) (M.keys m)

-- | Looks for the source vertices from a graph
sources = sinks . reverseG

fixInt :: Int -> Int
fixInt = id

main :: IO ()
main = do
    n <- fixInt <$> readLn
    adj <- replicateM n $ S.fromList . map read . init . words <$> getLine
    let g = G (M.fromList (zip [1..] adj))
    print g

