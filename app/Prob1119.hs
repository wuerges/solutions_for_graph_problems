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

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S

type V = (Int, Int)
type E = (V, V)

data G = G { neighM :: M.Map V [(V, Double)], diag :: S.Set V, n_dim :: Int, m_dim :: Int }
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
mkGraph ds n m = G { neighM = M.fromListWith (++) esN, diag = diags, n_dim = n, m_dim = m }
    where diags = S.fromList [(x, y) | [x, y] <- ds]
          es1 = mkEdges ds n m
          es2 = map (first swap) es1
          es  = S.toList . S.fromList $ (es1 ++ es2)
          esN = map (\((v1, v2), w) -> (v1, [(v2, w)])) es

readInt :: String -> Int
readInt = read

decomp :: V -> G -> Maybe ([(V, Double)], G)
decomp v g = do
    ns <- M.lookup v (neighM g)
    let g' = g { neighM = M.delete v (neighM g) }
    return (ns, g')


search :: G -> Q.Seq V -> Q.Seq (E, Double)
search g q | Q.null q = Q.empty
           | otherwise = case decomp v g of
    Nothing -> search g vs
    Just (ns, g') -> es >< search g' (vs >< qs)
        where qs = Q.fromList $ map fst ds
              ds = sortOn snd ns
              es = Q.fromList $ [((a, b), w) | (a, (b, w)) <- zip (repeat v) ds]
  where (v :< vs)  = Q.viewl q

type Dst =  M.Map V (Double, V)

calcSP :: Q.Seq (E, Double) -> Dst -> Dst
calcSP q rem | Q.null q  = rem
             | otherwise = case M.lookup a rem of
      Nothing     -> error "Could not find path for a"
      Just (w, t) -> case M.lookup b rem of
            Nothing -> calcSP es $ M.insert b (ew + w, a) rem
            Just (ow, op) -> if ow > ew + w
                                then calcSP es $ M.insert b (ew + w, a) rem
                                else calcSP es $ rem
  where (((a, b), ew) :< es) = Q.viewl q

shortestPaths :: G -> V -> Dst
shortestPaths g v = calcSP (search g $ Q.singleton v) (M.singleton v (0.0, v))

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
    let d = (fst $ shortestPaths g v0 ! (n, m))
    print $  round $ 100 * d

