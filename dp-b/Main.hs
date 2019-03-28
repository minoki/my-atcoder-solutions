import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

main = do
  [n,k] <- map read . words <$> getLine
  -- 2 <= n <= 10^5, 1 <= k <= 100
  h <- V.fromListN n . map read . words <$> getLine
  -- 1 <= hi <= 10^4
  let dp = V.create $ do
        dp <- VM.new n
        VM.write dp 0 0
        forM_ [1..n-1] $ \i -> do
          c <- forM [max 0 (i-k)..i-1] $ \j -> do
            c <- VM.read dp j
            return (c + abs (h V.! i - h V.! j))
          VM.write dp i (minimum c)
        return dp
  print (V.last dp :: Int)
