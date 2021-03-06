module Main where

import System.IO
import Control.Applicative
import Control.Monad
import Data.Tuple
import Data.Maybe
import qualified Data.List as L
import Data.Ord
import Control.Arrow
import Debug.Trace

import qualified Data.IntMap as M
import Data.IntMap ((!))
import qualified Data.IntSet as S

type V = Int
type E = (V, V)

mkV :: (Int, Int) -> Int
mkV (a, b) = (a * 10000) + b

data G = G { vertexM :: M.IntMap [(V, Double)], n_dim :: Int, m_dim :: Int }
    deriving Show

mkGraph :: [[Int]] -> Int -> Int -> G
mkGraph ds n m = G { vertexM = M.fromListWith (++) (d_w_edges ++ r_edges), n_dim = n, m_dim = m }
    where d_edges :: [((Int, Int), (Int,Int))]
          d_edges = [((x-1, y-1), (x, y)) | [x, y] <- ds]
          ends = (n, m) : (0,0) : map snd d_edges ++ map fst d_edges

          r_edges :: [(V, [(V, Double)])]
          r_edges = [(mkV e,[(mkV b, dist e b)]) | e <- ends, b <- ends]
          d_w_edges :: [(V, [(V, Double)])]
          d_w_edges = [(mkV a, [(mkV b, sqrt 2)]) | (a, b) <- d_edges]

          dist :: (Int, Int) -> (Int, Int) -> Double
          dist (x1, y1) (x2, y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)

readInt :: String -> Int
readInt = read

decomp :: V -> G -> [(V, Double)]
decomp v g = vertexM g ! v

search :: G -> S.IntSet -> [V] -> [(E, Double)]
search g vis []       = []
search g vis (v:vs)
               | S.member v vis = search g vis vs
               | otherwise =  es ++ search g (S.insert v vis) (vs ++ qs)
                    where ns = decomp v g
                          qs = map fst ds
                          ds = sortOn snd ns
                          es = [((a, b), w) | (a, (b, w)) <- zip (repeat v) ds]

type Dst =  M.IntMap (Double, V)

calcSP :: [(E, Double)] -> Dst -> Dst
calcSP q dst = foldl (flip calcSP1) dst q

calcSP1 :: (E, Double) -> Dst -> Dst
calcSP1 ((a, b), ew) rem = M.alter alterF b rem
    where (w, t) = rem ! a
          alterF Nothing                         = Just (ew + w, a)
          alterF (Just o@(ow, op)) | ow > ew + w = Just (ew + w, a)
                                   | otherwise   = Just o

shortestPaths :: G -> V -> Dst
shortestPaths g v = calcSP (search g S.empty $ [v]) (M.singleton v (0.0, v))

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = L.sortBy . comparing


main :: IO ()
main = do
    [n, m] <- map readInt . words <$> getLine
    d <- readLn
    ds <- replicateM d $ map readInt . words <$> getLine
    let g = mkGraph ds n m
    let v0 = (0, 0)
    let d = (fst $ shortestPaths g (mkV v0) ! mkV (n, m))
    print $  round $ 100 * d

