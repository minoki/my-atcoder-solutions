-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr, foldl')
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  [n,k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ sum @[] @Int $ do xs <- replicateM n [1..k]
                            pure $ foldl' gcd 0 xs
