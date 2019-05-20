{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Char8 as BS

main = do
  [n,k] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  print $ sum [ 1/fromIntegral n * (1/2)^l
              | d <- [1..n]
              , let q = (k+d-1) `quot` d
                    l = ceiling (logBase 2 (fromIntegral q) :: Double)
              ]
