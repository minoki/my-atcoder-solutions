-- https://github.com/minoki/my-atcoder-solutions

main = do
  s <- getLine
  print $ case s of
    "SUN" -> 7
    "MON" -> 6
    "TUE" -> 5
    "WED" -> 4
    "THU" -> 3
    "FRI" -> 2
    "SAT" -> 1
