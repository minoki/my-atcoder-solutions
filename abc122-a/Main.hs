main = do
  [b] <- getLine
  putStrLn $ case b of
    'A' -> "T"
    'C' -> "G"
    'G' -> "C"
    'T' -> "A"
