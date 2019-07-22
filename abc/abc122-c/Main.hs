{-# LANGUAGE BangPatterns #-}
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

build :: BS.ByteString -> V.Vector Int
build s = V.create $ do
  let !n = BS.length s
  vec <- VM.new n
  forM_ [n-2,n-3..0] $ \i -> do
    k <- VM.read vec (i + 1)
    if s `BS.index` i == 'A' && s `BS.index` (i+1) == 'C'
    then VM.write vec i (k+1)
    else VM.write vec i k
  return vec

main = do
  -- 2 <= n <= 10^5, 1 <= q <= 10^5
  [n,q] <- map read . words <$> getLine
  s <- BS.getLine
  let !t = build s
  sequence_ $ replicate q $ do
    -- 1 <= l < r < n
    [l,r] <- map read . words <$> getLine
    print $ (t V.! (l - 1)) - (t V.! (r - 1))
