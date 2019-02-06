{-# LANGUAGE BangPatterns #-}
import Data.Int
import Data.Char
import Data.List
import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

modulo :: Int64
modulo = 10^9 + 7

addMod x y = (x + y) `rem` modulo
subMod x y = (x - y) `mod` modulo

-- s: Nの最上位の桁からm+1桁目までの和、n: Nの上からm桁目
step :: Int -> Int -> Int -> V.Vector Int64 -> V.Vector Int64
step !d !s !n v = V.create $ do
  w <- VM.new d -- or V.new (max d (V.length v + 9))
  flip V.imapM_ v $ \i k -> do
    if s == i
      then do forM_ [0..n] $ \j -> do
                let l = (i + j) `rem` d
                VM.modify w (`addMod` k) l
              forM_ [n+1..9] $ \j -> do
                let l = (i + j) `rem` d
                VM.modify w (`addMod` (k - 1)) l
      else do forM_ [0..9] $ \j -> do
                let l = (i + j) `rem` d
                VM.modify w (`addMod` k) l
  return w

main :: IO ()
main = do
  !d <- readLn :: IO Int -- 1 <= d <= 100
  ns <- getLine -- 1 <= n <= 10^10000
  let n = map digitToInt ns :: [Int]
      loop !s [] !v = v
      loop !s (x:xs) !v = loop ((s+x) `rem` d) xs $ step d s x v
      result = loop 0 n (V.cons 1 $ V.replicate (d-1) 0)
  print $ (result V.! 0) `subMod` 1
