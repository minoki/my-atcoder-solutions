{-# LANGUAGE BangPatterns #-}
import qualified Data.Vector.Unboxed as V
import Data.Monoid ((<>))

main = do
  k <- readLn :: IO Int -- 1 <= k <= 10
  ratings <- V.replicateM (2^k) readLn :: IO (V.Vector Int) -- 0 <= Ri <= 4000

  let prob :: Int -> Int -> Double -- i番目の人がj番目の人に勝つ確率
      prob !i !j = 1 / (1 + 10 ** (fromIntegral (ratings V.! j - ratings V.! i) / 400))

      oneStep :: Int -> [V.Vector Double] -> [V.Vector Double]
      oneStep !i [] = []
      oneStep !i [x] = [x]
      oneStep !i (x:y:xs) = (V.imap (\j p -> p * V.ifoldl' (\s k q -> s + q * prob (i + j) (i + V.length x + k)) 0 y) x
                             <> V.imap (\j p -> p * V.ifoldl' (\s k q -> s + q * prob (i + V.length x + j) (i + k)) 0 x) y)
                            : oneStep (i + V.length x + V.length y) xs

      loop ::[V.Vector Double] -> V.Vector Double
      loop [] = error "error"
      loop [x] = x
      loop xs = loop (oneStep 0 xs)

      res = loop $ replicate (2^k) (V.singleton 1)

  V.forM_ res $ \p -> print p
