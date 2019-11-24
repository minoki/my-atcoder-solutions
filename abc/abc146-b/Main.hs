-- https://github.com/minoki/my-atcoder-solutions
import Data.Char
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  s <- BS.getLine
  let f c = chr $ (ord c - ord 'A' + n) `rem` 26 + ord 'A'
  BS.putStrLn $ BS.map f s
