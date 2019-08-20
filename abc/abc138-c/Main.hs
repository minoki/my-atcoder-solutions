-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr, sort, foldl1')
import Control.Monad
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn :: IO Int
  xs <- sort . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let result :: Double
      result = foldl1' (\x y -> (x + y) / 2) $ map fromIntegral xs
  print result
