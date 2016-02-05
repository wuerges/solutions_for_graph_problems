
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

{-
checkPart :: G -> [V] -> S.IntSet -> Bool
checkPart g vs s = all (\v -> checkP1 g v s) vs
    where checkP1 g v s = case decomp v g of
            Nothing      -> True
            Just (ns, _) -> if S.member v s
                               then all (\n -> not (S.member n s)) $ S.toList ns
                               else all (\n ->   S.member n s) $ S.toList ns
                               -}

readInt :: String -> Int
readInt = read

decomp :: V -> G -> Maybe (S.IntSet, G)
decomp v g = do
    ns <- M.lookup v (vertexM g)
    return (ns, G $ M.delete v (vertexM g))

anyVertex :: G -> Maybe V
anyVertex g = case M.toList $ vertexM g of
                []    -> Nothing
                ((v,_):_) -> Just v

bfs :: G -> [V] -> [V]
bfs g [] = case anyVertex g of
             Nothing -> []
             Just v  -> bfs g [v]
bfs g (v:vs) = case decomp v g of
                 Nothing       -> bfs g vs
                 Just (ns, g') -> v : bfs g' (S.toList ns ++ vs)

partition :: G -> [V] -> Maybe S.IntSet
partition g vs = do (_, s) <- foldM (flip (part1 g)) (S.empty, S.empty) vs
                    return s

part1 :: G -> V -> (S.IntSet, S.IntSet) -> Maybe (S.IntSet, S.IntSet)
part1 g v (red, blue) = case decomp v g of
                Nothing      -> error "Vertex should be in the graph"
                Just (ns, _) -> case (S.member v red, S.member v blue) of
                                  (True, False) -> if all (not . flip S.member red) (S.toList ns)
                                                      then Just (red, S.union ns blue)
                                                      else Nothing
                                  (False, True) -> if all (not . flip S.member blue) (S.toList ns)
                                                      then Just (S.union ns red, blue)
                                                      else Nothing
                                  (True, True)   -> Nothing
                                  (False, False) -> Just (S.insert v red, S.union ns blue)

colors :: S.IntSet -> Int -> String
colors s n = map (\i -> if S.member i s then '1' else '0') [1..n]


main :: IO ()
main = do
    d <- readLn
    ds <- replicateM d $ map readInt . init . words <$> getLine
    let g = mkGraph ds
    let vs = M.keys $ vertexM g
    let p = partition g (bfs g [1]) --vs
    case p of
    --case checkPart g vs p of
      Nothing -> print (-1)
      Just s  -> putStrLn $ colors s d

