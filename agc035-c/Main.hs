-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Bits

printEdge :: Int -> Int -> IO ()
printEdge !i !j = putStrLn $ show i ++ " " ++ show j

main = do
  n :: Int <- readLn
  if n .&. (n-1) == 0 -- n = 2^k for some k
    then putStrLn "No"
    else do putStrLn "Yes"
            let m = bit (floor (logBase 2 (fromIntegral (n+1))))
            forM_ [1..m-2] $ \i -> do
              printEdge i (i+1)
            printEdge (m-1) (n+1)
            forM_ [1..m-2] $ \i -> do
              printEdge (n+i) (n+i+1)
            when (n >= m + 1) $ do
              printEdge 1 m
              printEdge m (m+1)
              printEdge 1 (n+m+1)
              printEdge (n+m+1) (n+m)
              when (n >= m + 2) $ do
                printEdge 2 (m+2)
                printEdge (n+m+1) (n+m+2)
                when (n >= m + 3) $ do
                  printEdge m (m+3)
                  printEdge 2 (n+m+3)
                  when (n >= m + 4) $ do
                    printEdge m (n+m+4)
                    printEdge 4 (m+4)
                    forM_ [m+5..n] $ \i -> do
                      printEdge (i-1) (n+i)
                      printEdge (i-m) i
