-- https://github.com/minoki/my-atcoder-solutions

main = do
  s <- getLine
  putStrLn $ case s of
               "Sunny" -> "Cloudy"
               "Cloudy" -> "Rainy"
               "Rainy" -> "Sunny"
