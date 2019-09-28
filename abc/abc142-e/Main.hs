-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import Data.Bits

main = do
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  keys <- V.replicateM m $ do
    [a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    c <- U.unfoldrN b (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a,c)
  let v0 :: U.Vector Int
      v0 = U.generate (2^n) (\x -> if x == 0 then 0 else 10^9)
      result = V.foldl' (\v (a,c) -> U.generate (2^n) $ \x ->
                            if x == 0
                            then 0
                            else min (v U.! x) $ let cc = U.foldl' (\y i -> y .|. bit (i-1)) 0 c
                                                 in (v U.! (x .&. complement cc)) + a
                        ) v0 keys
  print $ if result U.! (2^n - 1) >= 10^9 then -1 else result U.! (2^n - 1)
