
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

newtype G = G { vertexM :: M.IntMap S.IntSet }
    deriving Show

mkGraph :: [[Int]] -> G
mkGraph ds = G { vertexM = M.fromListWith S.union [(a, S.singleton b) | (a,b) <- es ++ map swap es] }
    where
        es = concat $ zipWith (\a bs -> zip (repeat a) bs) [1..] ds


checkPart :: G -> [V] -> S.IntSet -> Bool
checkPart g vs s = all (\v -> checkP1 g v s) vs
    where checkP1 g v s = case decomp v g of
            Nothing      -> True
            Just (ns, _) -> if S.member v s
                               then all (\n -> not (S.member n s)) $ S.toList ns
                               else all (\n ->   S.member n s) $ S.toList ns

readInt :: String -> Int
readInt = read

decomp :: V -> G -> Maybe (S.IntSet, G)
decomp v g = do
    ns <- M.lookup v (vertexM g)
    return (ns, G $ M.delete v (vertexM g))


partition :: G -> [V] -> S.IntSet
partition g = foldl (flip (part1 g)) S.empty

part1 :: G -> V -> S.IntSet -> S.IntSet
part1 g v s | S.member v s = s
            | otherwise    = case decomp v g of
                               Nothing -> s
                               Just (ns, _) -> S.union s ns

colors :: S.IntSet -> Int -> String
colors s n = map (\i -> if S.member i s then '1' else '0') [1..n]


main :: IO ()
main = do
    d <- readLn
    ds <- replicateM d $ map readInt . init . words <$> getLine
    let g = mkGraph ds
    let vs = M.keys $ vertexM g
    let p = partition g vs
    if checkPart g vs p
       then putStrLn $ colors p d
       else print (-1)

