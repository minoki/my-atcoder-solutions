-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Data.Bifunctor (first)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap

main = do
  n <- readLn
  hs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  as <- U.unfoldrN n (readInt64 . BS.dropWhile isSpace) <$> BS.getLine
  let -- v ! i は j < i かつ hs ! j < hs ! i となるような j のうち、 hs ! j が最大のもの
      -- そのような j が存在しなければ -1
      v :: U.Vector Int
      v = U.create $ do
        vec <- UM.new n
        let loop !i !m
              | i == n = return ()
              | otherwise = do
                  let hs_i = hs U.! i
                  case IntMap.lookupLT hs_i m of
                    Just (hs_j, j) -> do
                      UM.write vec i j
                      loop (i+1) (IntMap.insert hs_i i m)
                    Nothing -> do
                      UM.write vec i (-1)
                      loop (i+1) (IntMap.insert hs_i i m)
        loop 0 IntMap.empty
        return vec
      -- resultV ! i は、入力の先頭から i+1 本のうち高さが hs ! i 以下のものを選んだ部分列に関する問題の答え
      resultV :: U.Vector Int64
      resultV = U.create $ do
        vec <- UM.new n
        let loop !i !m
              | i == n = return ()
              | otherwise = do
                  let h = hs U.! i
                  let a = as U.! i
                  let (left, right) = IntMap.split h m
                  let s' = maximum (0 : IntMap.elems left)
                  let s'' = a + s'
                  UM.write vec i s''
                  loop (i+1) $ IntMap.insert h s'' $ IntMap.union left $ IntMap.filter (> s'') right
        loop 0 IntMap.empty
        return vec
  print $ U.maximum $ resultV

readInt64 :: BS.ByteString -> Maybe (Int64, BS.ByteString)
readInt64 s = first fromIntegral <$> BS.readInt s
