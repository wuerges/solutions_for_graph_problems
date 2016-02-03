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
mkV = fixkey

data G = G { diag :: S.IntSet, n_dim :: Int, m_dim :: Int }
    deriving Show

validV :: Int -> Int -> V -> Bool
validV n m v = a <=n && b <= m
    where a = v `div` 10000
          b = v `mod` 10000

sides :: V -> [V]
--sides v = [first (+1) v, first (1-) v, second (+1) v, second (1-) v]
sides v = [v + 10000, v + 1]

mkGraph :: [[Int]] -> Int -> Int -> G
mkGraph ds n m = G { diag = diags, n_dim = n, m_dim = m }
    where diags = S.fromList [fixkey (x, y) | [x, y] <- ds]


readInt :: String -> Int
readInt = read

decomp :: V -> G -> [(V, Double)]
decomp v g = ns
       where ns = d2 ++ [(vi, 1) | vi <- sides v, validV (n_dim g) (m_dim g) vi]
             d2 = if S.member (ru v) (diag g) then [(ru v, sqrt 2)] else []
             ru ab = ab + 10001


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

fixkey :: (Int, Int) -> (Int)
fixkey (a, b) = (a * 10000 + b)

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
    let d = (fst $ shortestPaths g (fixkey v0) ! fixkey (n, m))
    print $  round $ 100 * d

