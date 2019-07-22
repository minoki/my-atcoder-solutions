{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Int
import qualified Data.ByteString.Char8 as BS

main = do
  [m,k] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 0 <= m <= 16, 0 <= k <= 10^9
  case m of
    0 | k == 0 -> putStrLn "0 0"
      | otherwise -> putStrLn "-1"
    1 | k == 0 -> putStrLn "0 0 1 1"
      | otherwise -> putStrLn "-1"
    _ | k < 2^m -> do let l = [0..2^m-1]
                          l' = filter (/= k) l
                      BS.putStrLn $ BS.unwords $ map (BS.pack . show) $ l' ++ [k] ++ reverse l' ++ [k]
      | otherwise -> putStrLn "-1"
