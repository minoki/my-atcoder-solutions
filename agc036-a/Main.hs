-- https://github.com/minoki/my-atcoder-solutions
import Data.Int (Int64)

check :: (Int64, Int64, Int64, Int64, Int64, Int64) -> Maybe Int64
check (x1,y1,x2,y2,x3,y3)
  | 0 <= x1 && x1 <= 10^9
  , 0 <= y1 && y1 <= 10^9
  , 0 <= x2 && x2 <= 10^9
  , 0 <= y2 && y2 <= 10^9
  , 0 <= x3 && x3 <= 10^9
  , 0 <= y3 && y3 <= 10^9 = let x2' = x2 - x1
                                y2' = y2 - y1
                                x3' = x3 - x1
                                y3' = y3 - y1
                            in Just (x2' * y3' - y2' * x3')
  | otherwise = Nothing

solve :: Int64 -> (Int64, Int64, Int64, Int64, Int64, Int64)
solve s | s == 10^18 = (0,0,10^9,0,0,10^9)
        | otherwise = let (a,b) = s `quotRem` (10^9)
                      in (0,0,10^9,1,10^9-b,a+1)

main = do
  s <- readLn
  let (x1,y1,x2,y2,x3,y3) = solve s
  putStrLn $ unwords $ map show [x1,y1,x2,y2,x3,y3]

{-# RULES
"^9/Int" forall x. x ^ (9 :: Int) = let u = x; v = u * u * u in v * v * v
"^9/Integer" forall x. x ^ (9 :: Integer) = let u = x; v = u * u * u in v * v * v
 #-}
