main = do
  [a,b,c,d] <- getLine
  let x = read [a,b] :: Int
      y = read [c,d] :: Int
  let xm = 1 <= x && x <= 12
      ym = 1 <= y && y <= 12
  putStrLn $ case (xm, ym) of
    (True, True) -> "AMBIGUOUS"
    (True, False) -> "MMYY"
    (False, True) -> "YYMM"
    (False, False) -> "NA"
