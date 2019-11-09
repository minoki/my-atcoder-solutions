-- https://github.com/minoki/my-atcoder-solutions
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn :: IO Int
  s <- BS.getLine
  print $ length $ BS.group s
