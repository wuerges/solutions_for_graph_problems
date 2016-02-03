module Main where

import System.IO
import Control.Applicative
import Control.Monad
import Data.Tuple
import Data.Maybe
import qualified Data.List as L
import qualified Data.Sequence as Q
import Data.Sequence ( ViewL(..), (><))
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

validV :: Int -> Int -> V -> Bool
validV n m v = a <=n && b <= m
    where a = v `div` 10000
          b = v `mod` 10000

sides :: V -> [V]
--sides v = [first (+1) v, first (1-) v, second (+1) v, second (1-) v]
sides v = [v + 10000, v + 1]

mkGraph :: [[Int]] -> Int -> Int -> G
mkGraph ds n m = G { vertexM = M.fromListWith (++) (d_w_edges ++ r_edges), n_dim = n, m_dim = m }
    where d_edges :: [((Int, Int), (Int,Int))]
          d_edges = [((x-1, y-1), (x, y)) | [x, y] <- ds]
          ends = (n, m) : (0,0) : map snd d_edges
          begs = (n,m) : (0,0) : map fst d_edges

          r_edges :: [(V, [(V, Double)])]
          r_edges = [(mkV e,[(mkV b, dist e b)]) | e <- ends, b <- begs]
          d_w_edges :: [(V, [(V, Double)])]
          d_w_edges = [(mkV a, [(mkV b, sqrt 2)]) | (a, b) <- d_edges]

          dist :: (Int, Int) -> (Int, Int) -> Double
          dist (x1, y1) (x2, y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)

readInt :: String -> Int
readInt = read

decomp :: V -> G -> [(V, Double)]
decomp v g = vertexM g ! v
 {-
decomp v g = ns
       where ns = d2 ++ [(vi, 1) | vi <- sides v, validV (n_dim g) (m_dim g) vi]
             d2 = if S.member (ru v) (diag g) then [(ru v, sqrt 2)] else []
             ru ab = ab + 10001
             -}


search :: G -> S.IntSet -> Q.Seq V -> Q.Seq (E, Double)
search g vis q | Q.null q       = Q.empty
               | S.member v vis = search g vis vs
               | otherwise =  es >< search g (S.insert v vis) (vs >< qs)
                    where ns = decomp v g
                          qs = Q.fromList $ map fst ds
                          ds = sortOn snd ns
                          es = Q.fromList $ [((a, b), w) | (a, (b, w)) <- zip (repeat v) ds]
                          (v :< vs)  = Q.viewl q

type Dst =  M.IntMap (Double, V)

calcSP :: Q.Seq (E, Double) -> Dst -> Dst
calcSP q dst = foldl (flip calcSP1) dst q
{-
calcSP q rem | Q.null q  = rem
             | otherwise = calcSP es $ M.alter alterF b rem
                      where (w, t) = rem ! a
                            (((a, b), ew) :< es) = Q.viewl q
                            alterF v = case v of
                                       Nothing         -> Just (ew + w, a)
                                       Just o@(ow, op) -> Just $ if ow > ew + w
                                                           then (ew + w, a)
                                                           else o
                                                           -}

calcSP1 :: (E, Double) -> Dst -> Dst
calcSP1 ((a, b), ew) rem = M.alter alterF b rem
    where (w, t) = rem ! a
          alterF Nothing                         = Just (ew + w, a)
          alterF (Just o@(ow, op)) | ow > ew + w = Just (ew + w, a)
                                   | otherwise   = Just o

shortestPaths :: G -> V -> Dst
shortestPaths g v = calcSP (search g S.empty $ Q.singleton v) (M.singleton v (0.0, v))

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = L.sortBy . comparing


main :: IO ()
main = do
    [n, m] <- map readInt . words <$> getLine
    d <- readLn
    ds <- replicateM d $ map readInt . words <$> getLine
    --print $ mkEdges ds n m
    let g = mkGraph ds n m
    let v0 = (0, 0)
    --let m0 = M.singleton v0 (0.0, v0) :: Dst
    --print g
    --print $ search g [v0]
    let d = (fst $ shortestPaths g (mkV v0) ! mkV (n, m))
    print $  round $ 100 * d

