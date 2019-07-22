{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Char8 as BS

main = do
  n :: Int <- readLn
  vs :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  cs :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  print $ sum $ filter (> 0) $ zipWith (-) vs cs
