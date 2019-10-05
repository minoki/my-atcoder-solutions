-- https://github.com/minoki/my-atcoder-solutions
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BS

main = do
  s <- BS.getLine
  k <- readLn :: IO Int64
  let ss = BS.group s
  let l = length ss
  let result | l == 1 = k * fromIntegral (BS.length s) `quot` 2
             | BS.head s == BS.last s = let ss0 = head ss
                                            ssz = last ss
                                        in fromIntegral (BS.length ss0 `quot` 2) + k * fromIntegral (sum $ map (\t -> BS.length t `quot` 2) $ init $ tail ss) + fromIntegral (BS.length ssz `quot` 2) + (k-1) * fromIntegral ((BS.length ss0 + BS.length ssz) `quot` 2)
             | otherwise = k * fromIntegral (sum $ map (\t -> BS.length t `quot` 2) ss)
  print result
