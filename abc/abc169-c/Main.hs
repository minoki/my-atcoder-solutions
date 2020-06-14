-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BS

main = do
  [a,b] <- BS.words <$> BS.getLine
  let Just (a',_) = BS.readInteger a
      a'' = fromIntegral a' :: Int64
      b' = read @Double (BS.unpack b)
  print $ a'' * round (b' * 100) `quot` 100
