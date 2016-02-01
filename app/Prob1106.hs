module Main where

import System.IO
import Control.Applicative
import Control.Monad
import Data.Tuple
import Control.Arrow

import qualified Data.IntMap as M
import qualified Data.Set as S


type GMap = M.IntMap (S.Set Int)
newtype G = G GMap
    deriving Show

-- | Decomposes a Map and key into a into a tuple
-- with its values and the map without the key
decomp :: Int -> GMap -> Maybe ([Int], GMap)
decomp k m = do
    vs <- M.lookup k m
    return (S.toList vs, M.delete k m)

readEdges :: Int -> IO [(Int, Int)]
readEdges n = do
    adj <- replicateM n $ map read . init . words <$> getLine
    return $ concat $ zipWith (\a bs -> zip (repeat a) bs) [1..] adj

graphFromEdges :: [(Int, Int)] -> G
graphFromEdges es = G $ M.fromListWith S.union $ mes
    where mes = map (second S.singleton) (es ++ map swap es)

neighs :: Int -> G -> S.Set Int
neighs n (G g) = case M.lookup n g of
                   Nothing -> S.empty
                   Just ns -> ns

greedyMatch :: G -> S.Set Int
greedyMatch (G g) = greedyMatch1 (G g) (M.keys g) S.empty

greedyMatch1 :: G -> [Int] -> S.Set Int -> S.Set Int
greedyMatch1 _  []     ls = ls
greedyMatch1 g  (p:ps) ls = if S.member p ls
        then greedyMatch1 g ps ls
        else greedyMatch1 g ps (ls `S.union` neighs p g)


main :: IO ()
main = do
    n <- readLn
    es <- readEdges n
    let g = graphFromEdges es
    let gm = greedyMatch g
    --print $ g
    print $ S.size gm
    putStrLn . unwords . map show . S.toList $ gm

