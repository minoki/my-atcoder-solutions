-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

solve :: U.Vector Int -> Int -> Either {- value -} Int {- index -} Int
solve v !x = let y = gcd x (U.last v)
             in if y == 1 then
                  Right $ search 0 (U.length v - 1)
                else
                  Left y
  where
    n = U.length v
    search i j
      -- i < j, gcd x (v U.! i) /= 1, gcd x (v U.! j) == 1
      | i >= j = error "invalid input"
      | j - i == 1 = j
      | otherwise = let k = i + (j - i) `quot` 2
                        y = gcd x (v U.! k)
                    in if y == 1 then
                         search i k
                       else
                         search k j

naive :: U.Vector Int -> Int -> Either {- value -} Int {- index -} Int
naive v x = case U.findIndex (\y -> gcd x y == 1) v of
              Nothing -> Left $ gcd x (U.last v)
              Just i -> Right i

main = do
  [n,q] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  ss <- U.unfoldrN q (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let gs = U.scanl gcd 0 as
  U.forM_ ss \s -> case solve gs s of
                     Left x -> print x
                     Right y -> print y
