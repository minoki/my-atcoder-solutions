{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Char8 as BS

main = do
  [n,k] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  print (n - k + 1)
