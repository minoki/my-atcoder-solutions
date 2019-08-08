{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U
-- import qualified Data.Vector.Unboxed.Mutable as UM

-- n := V.length v + 1
-- v ! i = 表が i 枚、裏が n - i 枚出る確率
solve :: U.Vector Double -> U.Vector Double
solve ps = U.foldl' step (U.singleton 1) ps
  where
    step !vec !p = U.zipWith (\u v -> p * u + (1 - p) * v) (0 `U.cons` vec) (vec `U.snoc` 0)

-- いくつか書き方を試したが、どれもそんなに変わらなさそう。（入力のサイズが比較的小さいから？）
-- 強いて言えば U.generate を使うやつが気持ち速い。
--
-- > U.zipWith (\u v -> p * u + (1 - p) * v) (0 `U.cons` vec) (vec `U.snoc` 0)
--
-- > U.create $ do
-- >   let !n = U.length vec
-- >   vec2 <- UM.new (n + 1)
-- >   UM.write vec2 0 $ (1 - p) * vec U.! 0
-- >   U.copy (UM.init $ UM.tail vec2) $ U.zipWith (\u v -> p * u + (1 - p) * v) vec (U.tail vec)
-- >   UM.write vec2 n $ p * vec U.! (n - 1)
-- >   return vec2
--
-- > U.generate (U.length vec + 1) $ \i ->
-- >   if i == 0
-- >   then (1 - p) * vec U.! i
-- >   else if i == U.length vec
-- >        then p * vec U.! (i - 1)
-- >        else p * vec U.! (i - 1) + (1 - p) * vec U.! i
--
-- 結局、
-- > map (read . BS.unpack) . BS.words
-- を
-- > U.unfoldrN n (readDoubleBS . BS.dropWhile isSpace)
-- に変えるのが一番効果があった (16ms程度）といういつものやつ

main = do
  n <- readLn
  -- 1 <= n <= 2999, n is odd
  ps <- U.unfoldrN n (readDoubleBS . BS.dropWhile isSpace) <$> BS.getLine
  let result = solve ps
  -- U.length result == n + 1
  print $ U.sum $ U.drop (n `quot` 2 + 1) result

readDoubleBS :: BS.ByteString -> Maybe (Double, BS.ByteString)
readDoubleBS s = case BS.readInt s of
                   Just (ipart, s') -> case BS.uncons s' of
                                         Just ('.', s'') -> case BS.readInt s'' of
                                                              Just (fpart, s''') ->
                                                                let !l = BS.length s'' - BS.length s'''
                                                                    !x | ipart >= 0 = fromIntegral ipart + fromIntegral fpart / 10^l
                                                                       | otherwise = fromIntegral ipart - fromIntegral fpart / 10^l
                                                                in Just (x, s''')
                                                              Nothing -> Just (fromIntegral ipart, s')
                                         _ -> Just (fromIntegral ipart, s')
                   Nothing -> Nothing
