{-# LANGUAGE BangPatterns #-}
import System.Exit
import Control.Monad
import Data.List
import Data.Bits

main = do
  n <- readLn :: IO Int
  let parseInts s = case reads s of
                      [(x,t)] -> x : parseInts t
                      _ -> []
  ps <- (parseInts <$> getLine) :: IO [Int]
  when (length ps /= n) exitFailure
  print $ popCount (foldl' (\xs p -> (xs .|. (xs `shiftL` p))) 1 ps :: Integer)
