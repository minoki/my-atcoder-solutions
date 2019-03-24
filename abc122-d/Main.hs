import Data.Int
import Data.List
import Control.Monad

modulo :: Int64
modulo = 10^9 + 7
newtype N = N Int64 deriving Eq
instance Show N where
  show (N x) = show x
instance Num N where
  N x + N y = N ((x + y) `rem` modulo)
  N x - N y = N ((x - y) `mod` modulo)
  N x * N y = N ((x * y) `rem` modulo)
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined

solution :: [(N,N,N)]
solution = (1,0,0):(4,0,1):(16,1,4):(61,3,15)
  : zipWith4 (\(a1,b1,c1) (a2,b2,c2) (a3,b3,c3) (a4,b4,c4) -> (4*a1-3*a3+b3+c3-3*a4,a2-c2,a1-b1)) (drop 3 solution) (drop 2 solution) (tail solution) solution

main = do
  n <- readLn :: IO Int
  print $ case solution !! n of
            (N x,_,_) -> x

--
-- Naive solution:
--

gen :: Int -> [String]
gen n = sequence $ replicate n "ATCG"

swapOne :: String -> [String]
swapOne [] = []
swapOne [_] = []
swapOne (x0:xs@(x1:xss)) = (x1:x0:xss) : ((x0:) <$> swapOne xs)

maybeSwapOne :: String -> [String]
maybeSwapOne xs = xs : swapOne xs

includesAGC :: String -> Bool
includesAGC [] = False
includesAGC ('A':'G':'C':_) = True
includesAGC (x:xs) = includesAGC xs

numberOfStringsIncludingAGC :: Int -> Int
numberOfStringsIncludingAGC n = length $ filter includesAGC $ gen n
-- map numberOfStringsIncludingAGC [0..]
-- = [0,0,0,1,8,48,255,1268,6048,...]

numberOfStringsIncludingAGCBySwappingOne :: Int -> Int
numberOfStringsIncludingAGCBySwappingOne n = length $ filter (\s -> not (includesAGC s) && any includesAGC (swapOne s)) $ gen n
-- map numberOfStringsIncludingAGCBySwappingOne [0..]
-- = [0,0,0,2,18,111,594,2931,13769]

numberOfStringsIncludingAGCByMaybeSwappingOne :: Int -> Int
numberOfStringsIncludingAGCByMaybeSwappingOne n = length $ filter (any includesAGC . maybeSwapOne) $ gen n
-- map numberOfStringsIncludingAGCByMaybeSwappingOne [0..]
-- = [0,0,0,3,26,159,849,4199,19817,...]

naiveSolution :: Int -> Int
naiveSolution n = 4^n - numberOfStringsIncludingAGCByMaybeSwappingOne n
-- map naiveSolution [0..] = [1,4,16,61,230,865,3247,12185,45719,171531,...]

-- map length naiveSolution2 !! n
naiveSolution2 :: [[String]]
naiveSolution2 = iterate f [""]
  where f set = filter (not . excl) $ liftM2 (:) "ACGT" set
        -- excl = any includesAGC . maybeSwapOne . take 6
        excl ('A':'G':'C':_) = True
        excl ('G':'A':'C':_) = True
        excl ('A':'C':'G':_) = True
        excl ('A':'T':'G':'C':_) = True
        excl ('A':'G':'T':'C':_) = True
        excl ('A':'G':'G':'C':_) = True
        excl _ = False
-- 1,4,16,61,230,865,3247,12185,45719,171531,643550,2414454,9058467,33985227,

startsWithAC, startsWithG :: String -> Bool
startsWithAC ('A':'C':_) = True
startsWithAC _ = False
startsWithG ('G':_) = True
startsWithG _ = False
