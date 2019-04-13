{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Char8 as BS

main = do
  [a,b] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  if a == b
    then print $ a + b
    else print $ 2 * max a b - 1
