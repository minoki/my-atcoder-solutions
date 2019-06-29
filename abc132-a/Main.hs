-- https://github.com/minoki/my-atcoder-solutions
import Data.List

main = do
  [a,b,c,d] <- sort <$> getLine
  putStrLn $ if a == b && b /= c && c == d
             then "Yes"
             else "No"
