import Data.Int
import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

solve :: Int -> Int -> V.Vector (Int,Int) -> V.Vector Int64
solve n m edges = V.create $ do
  assocs <- VM.new n
  forM_ [0..n-1] $ \i -> VM.write assocs i i
  numberOfElements <- VM.replicate n 1
  let getRoot i xs = do
        j <- VM.read assocs i
        if i == j
          then return (i, xs)
          else getRoot j (i:xs)
      unify i j = do
        (i',xs) <- getRoot i []
        (j',ys) <- getRoot j []
        let k = min i' j'
        forM_ (i':xs) (\l -> VM.write assocs l k)
        forM_ (j':ys) (\l -> VM.write assocs l k)
        if i' == j'
          then return 0
          else do
          n1 <- VM.read numberOfElements i'
          n2 <- VM.read numberOfElements j'
          VM.write numberOfElements k (n1 + n2)
          return (n1 * n2)
  result <- VM.new m
  VM.write result (m-1) $ fromIntegral n * (fromIntegral n - 1) `quot` 2
  forM_ [m-1,m-2..1] $ \i -> do
    let (a,b) = edges V.! i
    convenience <- unify a b
    prevInconveniences <- VM.read result i
    VM.write result (i-1) (prevInconveniences - convenience)
  return result

main = do
  [n,m] <- map read . words <$> getLine -- 2 <= n <= 10^5, 1 <= m <= 10^5
  edges <- V.replicateM m $ do
    [a,b] <- map read . words <$> getLine
    return (a-1, b-1)
  -- maxIncovenience < n * (n - 1) / 2 < 10^10 (2^32 < 10^10)
  let result = solve n m edges
  V.forM_ result print
