-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BS

main = do
  s <- BS.getLine
  let gg = map (\t -> (BS.head t, fromIntegral $ BS.length t)) $ BS.group s
      g = case gg of
            ('>',_):_ -> ('<',0) : gg
            _ -> gg
      loop :: Int64 -> [(Char,Int64)] -> Int64
      loop !acc [] = acc
      loop !acc (('<',a):('>',b):xs) = loop (acc + a * (a-1) `quot` 2 + b * (b-1) `quot` 2 + max a b)xs
      loop !acc [('<',a)] = acc + a * (a+1) `quot` 2
  print $ loop 0 g
