{-# LANGUAGE BangPatterns #-}
import Data.Int
import Data.Monoid
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map

main = do
  n <- readLn
  as <- V.fromListN n . map (read . BS.unpack) . BS.words <$> BS.getLine
  let m :: Map.Map Int64 Int
      m = snd $ V.foldl' (\(!s,!m) a -> let !s' = s + a in (s', Map.insertWith (+) s' 1 m)) (0 :: Int64, Map.singleton 0 1) as
  print $ getSum $ Map.foldMapWithKey (\k n -> Sum (n * (n - 1) `quot` 2)) m
