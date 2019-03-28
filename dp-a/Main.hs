import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn -- 2 <= n <= 10^5
  h <- V.fromListN n . map (read . BS.unpack) . BS.words <$> BS.getLine
  -- hi <= 10^4
  let dp = V.create $ do
        dp <- VM.new n
        VM.write dp 0 0
        VM.write dp 1 (abs (h V.! 0 - h V.! 1))
        forM_ [2..n-1] $ \i -> do
          c2 <- VM.read dp (i - 2)
          c1 <- VM.read dp (i - 1)
          let c2' = c2 + abs (h V.! i - h V.! (i - 2))
              c1' = c1 + abs (h V.! i - h V.! (i - 1))
          VM.write dp i (min c1' c2')
        return dp
  print (V.last dp :: Int)
