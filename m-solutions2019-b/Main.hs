-- https://github.com/minoki/my-atcoder-solutions
import qualified Data.ByteString.Char8 as BS

main = do
  s <- BS.getLine
  let x = BS.count 'x' s
  if x <= 7
    then putStrLn "YES"
    else putStrLn "NO"
