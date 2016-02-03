module Main where

import System.IO
import Control.Applicative
import Control.Monad
import Data.Tuple
import Data.Maybe
import Data.List
import Control.Arrow
import Debug.Trace

import qualified Data.Map as M
import qualified Data.Set as S

type V = (Int, Int)
type E = (V, V)

data G = G { edgeM :: M.Map E Double, neighM :: M.Map V [(V, Double)] }
    deriving Show

validV :: Int -> Int -> V -> Bool
validV n m (a, b) = a >= 0 && b >= 0 && a <=n && b <= m

sides :: V -> [V]
sides v = [first (+1) v, first (1-) v, second (+1) v, second (1-) v]

mkEdges :: [[Int]] -> Int -> Int -> [(E, Double)]
mkEdges ds n m = des ++ ses
    where des = zip [((x, y), (x-1, y-1)) | [x, y] <- ds] (repeat (sqrt 2))
          es v = zip (repeat v) (filter (validV n m) (sides v))
          vs = [(x, y) | x <- [0..n], y <- [0..m]]
          ses = zip  (concatMap es vs) (repeat 1)

mkGraph :: [[Int]] -> Int -> Int -> G
mkGraph ds n m = G { edgeM = M.fromList es, neighM = M.fromListWith (++) esN }
    where es1 = mkEdges ds n m
          es2 = map (first swap) es1
          es  = (es1 ++ es2)
          esN = S.toList . S.fromList $ map (\((v1, v2), w) -> (v1, [(v2, w)])) es

readInt :: String -> Int
readInt = read

decomp :: V -> G -> Maybe ([(V, Double)], G)
decomp v g = do
    ns <- M.lookup v (neighM g)
    let g' = g { neighM = M.delete v (neighM g) }
    return (ns, g')

type Dst =  M.Map V (V, Double)

search :: G -> [V] -> [(E, Double)]
search g []     = []
search g (v:vs) = case decomp v g of
    Nothing -> search g vs
    Just (ns, g') -> es ++ search g' (vs ++ map fst ds)
        where ds = sortOn snd ns
              es = [((a, b), w) | (a, (b, w)) <- zip (repeat v) ds] :: [(E, Double)]

main :: IO ()
main = do
    [n, m] <- map readInt . words <$> getLine
    d <- readLn
    ds <- replicateM d $ map readInt . words <$> getLine
    --print $ mkEdges ds n m
    let g = mkGraph ds n m
    let v0 = (0, 0)
    print g
    print $ search g [v0]

