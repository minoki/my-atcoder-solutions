import qualified Data.ByteString.Char8 as BS

main = do
  [a,b,k] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  let d = gcd a b
  print $ [i | i <- [d,d-1..1], a `rem` i == 0, b `rem` i == 0] !! (k - 1)
