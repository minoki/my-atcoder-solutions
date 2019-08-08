{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Char
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS

readIntPair :: BS.ByteString -> (Int, Int)
readIntPair s = let Just (a, s') = BS.readInt s
                    Just (b, _) = BS.readInt $ BS.dropWhile isSpace s'
                in (a, b)

main = do
  (n,k) <- readIntPair <$> BS.getLine
  -- 2 <= n <= 10^5, 1 <= k <= 100
  h <- V.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- 1 <= hi <= 10^4
  let dp = V.constructN n $ \dp ->
        if V.null dp
        then 0
        else let !i = V.length dp
                 !h_i = h V.! i
             in V.minimum $ V.map (\(dp_j, h_j) -> dp_j + abs (h_i - h_j)) $ V.drop (i-k) $ V.zip dp h
  print (V.last dp :: Int)
