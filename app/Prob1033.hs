module Main where

import System.IO
import Control.Applicative
import Control.Monad
import Data.Tuple
import Data.List
import Control.Arrow

import qualified Data.Map as M
import qualified Data.Set as S


type Tab = [[Char]]

getTab :: IO (Int, Tab)
getTab = do
    n <- readLn
    t <- replicateM n $ getLine
    return (n, t)


checkLines :: String -> String -> [Int]
checkLines as bs =
    map fst $ filter snd $ zipWith3 (\n a b -> (n, a == '.' && b == '.')) [1..] as bs

type Vertex = (Int, Int)
type Edge = ((Int, Int), (Int, Int))

mkEdges1 :: Tab -> [Edge]
mkEdges1 t = zip v1s v2s
    where vs  = zipWith checkLines t (tail t)
          v1s = concat $ zipWith (\i v -> zip (repeat i) v) [1..] vs
          v2s = concat $ zipWith (\i v -> zip (repeat i) v) [2..] vs

mkEdges :: Tab -> [Edge]
mkEdges t = mkEdges1 t ++ (map swap2 $ mkEdges1 (transpose t))
    where swap2 ((a, b), (c, d)) = ((b, a), (d, c))

type G = M.Map Vertex (S.Set Vertex)

mkGraph :: [Edge] -> G
mkGraph = M.fromListWith S.union . map (second S.singleton)


neighs :: Vertex -> G -> [Vertex]
neighs v g = case M.lookup v g of
               Nothing -> []
               Just ns -> S.toList ns

dfs :: [Vertex] -> G -> S.Set Vertex
dfs []     g = S.empty
dfs (v:vs) g = v `S.insert` dfs (vs ++ neighs v g) (M.delete v g)
    -- until done step (S.singleton (1, 1), mkEdges t)

step :: (S.Set Vertex, [Edge]) -> (S.Set Vertex, [Edge])
step (v, [])     = (v, [])
step (v, (e:es)) | S.member (fst e) v = (S.insert (snd e) v, es)
                 | otherwise          = (v, es)

done :: (S.Set Vertex, [Edge]) -> Bool
done (v, []) = True
done _       = False


findWalls :: [Vertex] -> G -> [Int]
findWalls vs g = walls
    where walls = map ((4-) . length . flip neighs g) vs

numWalls :: [Int] -> Int
numWalls ws = sum ws - 4

main :: IO ()
main = do
    (n, t) <- getTab
    let es = mkEdges t
    let g = mkGraph $ es ++ (map swap es)
    let tiles = S.toList $ dfs [(1,1), (n, n)] g
    print $ (numWalls $ findWalls tiles g) * 3 * 3

