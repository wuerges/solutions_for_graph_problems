import System.IO
import Control.Monad


data T = L | T T Int T
    deriving Show

mkT :: [Int] -> T
mkT = foldl insertT L

insertT :: T -> Int -> T
insertT L           nk          = T L nk L
insertT t@(T l k r) nk | nk == k = t
                       | nk < k = T (insertT l nk) k r
                       | nk > k = T l              k (insertT r nk)

postT :: T -> [Int]
postT L         = []
postT (T l k r) = k : postT l ++ postT r


main = do n <- readLn
          ps <- replicateM n readLn
          let t = mkT $ reverse ps
          --print t
          --print $ postT t
          mapM_ print $ reverse $ postT t

