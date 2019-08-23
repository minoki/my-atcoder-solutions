-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (ord)
import Data.Int (Int64)
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.Sequence as Seq

buildTable :: BS.ByteString -> V.Vector (Seq.Seq Int)
buildTable s = let s' = V.generate (BS.length s) $ \i -> BS.index s i
               in V.scanr (\(i,c) a -> Seq.update (ord c - ord 'a') i a) (Seq.replicate 26 (-1)) (V.indexed s')

main = do
  s <- BS.getLine
  t <- BS.getLine
  let s_table = buildTable s
  let loop :: Int64 -> Int -> BS.ByteString -> Int64
      loop !i !j t = case BS.uncons t of
                       Nothing -> i
                       Just (c, t') ->
                         let k = (s_table V.! j) `Seq.index` (ord c - ord 'a')
                         in if k == -1
                            then let l = V.head s_table `Seq.index` (ord c - ord 'a')
                                 in if l == -1
                                    then -1
                                    else loop (i + fromIntegral (l + BS.length s - j) + 1) (l+1) t'
                            else loop (i + fromIntegral (k - j) + 1) (k+1) t'
  print $ loop 0 0 t
