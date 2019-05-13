{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Char8 as BS

main = do
  [r,g,b,n] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- n <= 3000
  print (sum [ 1
             | r' <- [0..n `quot` r]
             , g' <- [0..n `quot` g]
             , r * r' + g * g' <= n
             , (n - (r * r' + g * g')) `rem` b == 0
             ] :: Int)
