import Data.Char
import Data.Monoid
import qualified Data.ByteString.Char8 as BS

main = do
  [n,k] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  s <- BS.getLine
  BS.putStrLn $ BS.take (k - 1) s <> BS.singleton (toLower $ BS.index s (k - 1)) <> BS.drop k s
