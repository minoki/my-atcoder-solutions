{-# LANGUAGE BangPatterns #-}
import Data.Int
import Data.Monoid
import qualified Data.Vector.Unboxed as V

solve :: V.Vector Int -> [Int] -> Maybe [Int]
solve v res | V.null v = Just res
solve v res = case V.find (\(i,x) -> i == x) (V.reverse (V.indexed v)) of
                Nothing -> Nothing
                Just (i,x) -> solve (V.take i v <> V.drop (i + 1) v) (i : res)

main = do
  n <- readLn :: IO Int -- 1 <= n <= 100
  bs <- map (subtract (1 :: Int) . read) . words <$> getLine
  let cond = and $ zipWith (>=) [0..] bs
  if cond
    then case solve (V.fromListN n bs) [] of
           Just xs -> mapM_ (print . (+1)) xs
           Nothing -> putStrLn "-1"
    else putStrLn "-1"
