-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap

main = do
  n <- readLn
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let m = IntMap.fromListWith (+) $ U.toList $ U.map (\x -> (x, 1 :: Int)) xs
  let result | IntMap.size m <= 3 = case IntMap.toList m of
                                      [(a0,_)] -> a0 == 0
                                      [(a0,n0),(a1,n1)] | n0 == 2 * n1 -> a1 == 0
                                                        | n1 == 2 * n0 -> a0 == 0
                                                        | otherwise -> False
                                      [(a0,n0),(a1,n1),(a2,n2)] -> n0 == n1 && n1 == n2 && a0 == a1 `xor` a2
             | otherwise = False
  putStrLn $ if result then "Yes" else "No"
