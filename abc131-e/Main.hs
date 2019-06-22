-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS

main = do
  [n,k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let m = (n - 1) * (n - 2) `quot` 2
  if k > m
    then putStrLn "-1"
    else do let l = m - k
            print $ n - 1 + l
            forM_ [2..n] $ \i -> do
              putStrLn $ unwords ["1", show i]
            let loop 0 !i !j = return ()
                loop l !i !j | j > n = loop l (i+1) (i+2)
                           | otherwise = do putStrLn $ unwords [show i, show j]
                                            loop (l - 1) i (j + 1)
            loop l 2 3
