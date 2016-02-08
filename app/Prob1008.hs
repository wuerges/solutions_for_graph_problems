import System.IO
import Control.Monad
import Control.Applicative
import Debug.Trace
import Data.Maybe
import Data.List
import qualified Data.Set as S

newtype ST s a = S { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> ST s a
state f = S { runState = f }

type V     = (Int, Int)
type MST a = ST (S.Set V) a
data T     = N | L V | T V T T T T
    deriving Show

bot   (a, b) = (a    , b - 1)
right (a, b) = (a + 1, b    )
top   (a, b) = (a    , b + 1)
left  (a, b) = (a - 1, b    )


get :: ST a a
get = state (\s -> (s, s))

put :: s -> ST s ()
put s = state (\_ -> ((), s))

{-
instance Functor (ST s) where
    fmap = error "fmap undefined"

instance Applicative (ST s) where
    pure = error "pure undefined"
    a <*> b = error "<*> undefined"
-}

instance Monad (ST s) where
    return x = state (\s -> (x, s))
    pr >>= k = state $ \ st ->
           let (x, st') = runState pr st -- Running the first processor on st.
           in runState (k x) st'       -- Running the second processor on st'.

try :: V -> (V -> V) -> MST (Maybe V)
try v d = do s <- get
             --if traceShow ("trying v", v) $ S.member (d v) s
             if S.member (d v) s
                then do
                    put $ S.delete (d v) s
                    return $ Just (d v)
                else return Nothing

neighs :: Maybe V -> MST T
neighs Nothing  = return N
neighs (Just v) = do
    mds <- mapM (try v) [right, top, left, bot]
    case mds of -- traceShow mds mds of
      [Nothing, Nothing, Nothing, Nothing] -> return $ L v
      _ -> do ls@[l1, l2, l3, l4] <- mapM neighs mds
              return $ T v l1 l2 l3 l4

readInt :: String -> Int
readInt = read


letters :: T -> [String]
letters N             = []
letters (L _)         = [""]
letters (T _ r t l b) = [cs]
    where cs = concatMap fchar $ zip "RTLB" [r, t, l, b]
          fchar (_, N)   = ""
          fchar (c, _)   = [c]

encode' :: T -> [String]
encode' N             = [] --error "should not encode N"
encode' (L _)         = []
encode' tn@(T v r t l b) = (concatMap letters bs) ++ concatMap encode' bs
    where bs = [r, t, l, b]

encode :: T -> String
encode t@(T (a, b) _ _ _ _)  = show a ++ " " ++ show b ++ "\n" ++ concat (intersperse ",\n" (letters t ++ encode' t)) ++ "."

main = do
    n <- readLn
    (px:pxs) <- replicateM n $
        (\[a, b] -> (a,b)) . map readInt . words <$> getLine
    --print (px:pxs)
    let t = fst $ runState (neighs $ Just px) (S.fromList pxs)
    --print t
    putStrLn $ encode t
