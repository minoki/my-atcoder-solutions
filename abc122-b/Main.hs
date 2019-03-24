import Data.List

main = do
  s <- getLine
  print $ maximum $ map (length . takeWhile (`elem` "ATGC")) (tails s)
