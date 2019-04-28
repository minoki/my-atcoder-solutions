{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Char8 as BS

main = do
  [a,b,t] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  print $ b * (t `quot` a)
