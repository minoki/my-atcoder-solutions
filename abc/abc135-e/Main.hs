-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import Data.Bifunctor
import Control.Exception
import System.IO
import Data.Monoid

-- d <= 2 * k && x >= 0 && y >= 0 && (even (x + y) || odd k)
solveSmall :: Int -> Int -> Int -> [(Int,Int)]
solveSmall !k !x !y
  | assert (x >= 0 && y >= 0 && d <= 2 * k) False = undefined
  | d == 0 = []
  | d == k = [(x,y)]
  | odd (x + y) && d < k =
    let !x1 = -1
        !y1 = 1-k
    in (x1,y1) : map (bimap (+ x1) (+ y1)) (solveSmall k (x-x1) (y-y1))
  | odd (x + y) {- k < d < 2*k -} =
    let (x1,y1) | x >= k = (k,0)
                | y >= k = (0,k)
                | otherwise = (x,k-x) -- k < x + y && x < k && y < k
    in (x1,y1) : map (bimap (+ x1) (+ y1)) (solveSmall k (x-x1) (y-y1))
  | otherwise {- even (x + y) -} =
    if x >= y
    then let !x1 = (x + y) `quot` 2
             !y1 = x1 - k
         in [(x1,y1), (x,y)]
    else let !y1 = (x + y) `quot` 2
             !x1 = y1 - k
         in [(x1,y1), (x,y)]
  where d = abs x + abs y -- <= 2 * k

-- x >= 0 && y >= 0
solve :: Int -> Int -> Int -> [(Int,Int)]
solve !k !x !y
  | d < 2 * k = solveSmall k x y
  -- Now x + y >= 2 * k holds, which implies x >= k || y >= k
  | otherwise =
    let (q,r) = x `quotRem` k -- 0 <= r < k
        (q',r') = y `quotRem` k -- 0 <= r' < k
        -- abs (q*k - x) + abs (q'*k - y)
        --   = r + r' < 2*k-1
        (m,n) | 0 < r + r' && r + r' < k =
                -- 2*k <= x + y = (q+q')*k + r+r'
                -- k < 2*k - (r+r') <= (q+q')*k
                -- Therefore, 1 < q+q'
                if q > q'
                then (q-1,q')
                else (q,q'-1)
              | otherwise {- k <= r+r' -} = (q,q')
        !x1 = m*k
        !y1 = n*k
    in [(i*k,0) | i <- [1..m]] ++ [(x1,j*k) | j <- [1..n]] ++ map (bimap (+ x1) (+ y1)) (solveSmall k (x-x1) (y-y1))
  where d = abs x + abs y

main = do
  k <- readLn
  [x,y] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  if k >= 2 && even k && odd (x + y)
    then putStrLn "-1"
    else do let xs = solve k (abs x) (abs y)
                ys | x < 0 && y < 0 = map (bimap negate negate) xs
                   | x < 0 = map (first negate) xs
                   | y < 0 = map (second negate) xs
                   | otherwise = xs
            -- print $ check k ys
            print $ length ys
            -- forM_ ys $ \(x',y') -> putStrLn $ unwords [show x', show y']
            BSB.hPutBuilder stdout $ mconcat $ map (\(x',y') -> BSB.intDec x' <> BSB.char7 ' ' <> BSB.intDec y' <> BSB.char7 '\n') ys

check :: Int -> [(Int,Int)] -> Bool
check !k xs = and $ zipWith (\(x0,y0) (x1,y1) -> abs (x0-x1) + abs (y0-y1) == k) ((0,0) : xs) xs
