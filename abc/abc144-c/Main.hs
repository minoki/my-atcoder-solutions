-- https://github.com/minoki/my-atcoder-solutions
import Data.Int (Int64)

main = do
  n <- readLn :: IO Int64
  print $ minimum [ a + b - 2
                  | d <- [1..floor (sqrt (fromIntegral n) :: Double)]
                  , n `rem` d == 0
                  , let a = d; b = n `quot` d
                  ]
