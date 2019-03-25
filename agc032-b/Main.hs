import Data.Int
import Control.Monad

main = do
  n <- readLn :: IO Int -- 3 <= n <= 100
  let m | even n = n + 1
        | otherwise = n
  print $ n * (n - 1) `quot` 2 - (m - 1) `quot` 2
  forM_ [1..n-1] $ \i -> do
    forM_ [i+1..n] $ \j -> do
      when (i + j /= m) $ putStrLn $ show i ++ " " ++ show j
