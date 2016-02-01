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

main :: IO ()
main = do
    n <- readLn
    es <- readEdges n
    print $ graphFromEdges es

