import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Char8 as BS

main = do
  [n,maxWeight] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 1 <= n <= 100, 1 <= maxWeight <= 10^9
  items <- V.replicateM n $ do
    [wi,vi] <- map (read . BS.unpack) . BS.words <$> BS.getLine
    -- 1 <= wi <= w, 1 <= vi <= 10^3
    return (wi, vi :: Int)
  let totalValue = V.sum $ snd $ V.unzip items -- 10^3 * 100 = 10^5
      result :: V.Vector Int
      result = V.dropWhile (== maxWeight + 1) $ V.reverse $ V.foldl'
               (\vec (wi,vi) ->
                  -- vec ! v : 価値 v を得るために必要な最小の重さ
                  V.generate (totalValue + 1)
                                (\v -> let w0 = fromMaybe 0 (vec V.!? v)
                                           w1 = fromMaybe 0 (vec V.!? (v - vi))
                                       in if w1 + wi <= maxWeight
                                          then min w0 (w1 + wi)
                                          else w0))
               (V.cons 0 $ V.replicate totalValue (maxWeight + 1)) items
  print $ V.length result - 1
