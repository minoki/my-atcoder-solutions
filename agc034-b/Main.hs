-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BS

countBCs :: BS.ByteString -> (Int, BS.ByteString)
countBCs s = loop 0 s
  where
    loop !i s | "BC" `BS.isPrefixOf` s = loop (i+1) (BS.drop 2 s)
              | otherwise = (i, s)

parse :: BS.ByteString -> [[(Int, Int)]]
parse s = doParse [] s
  where
    doParse acc s = case BS.uncons s of
      Nothing -> [reverse acc]
      Just ('A', xs) -> let (as, xs') = BS.span (== 'A') xs
                            (bcs, xs'') = countBCs xs'
                        in if bcs == 0
                           then reverse acc : doParse [] xs''
                           else doParse ((BS.length as + 1, bcs) : acc) xs''
      Just (_, xs) -> reverse acc : doParse [] xs

calc :: [(Int, Int)] -> Int
calc xs = loop 0 0 xs
  where
    loop :: Int -> Int -> [(Int,Int)] -> Int
    loop !s !sa [] = s
    loop !s !sa ((a,b):xs) = loop (s + (sa + a) * b) (sa + a) xs

main = do
  s <- BS.getLine
  print $ sum $ map calc $ parse s
