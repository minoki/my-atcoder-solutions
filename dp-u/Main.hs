-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Bifunctor (first)
import Data.Bits

type Mat = V.Vector (U.Vector Int64)
type BitSet = Int

{-
scoreOne :: Int -> Mat -> BitSet -> Int64
scoreOne !n !mat !set = go 0 [i | i <- [0..n-1], testBit set i]
  where
    go !acc [] = acc
    go !acc (x:xs) = let !r = mat V.! x
                     in go (acc + sum [r U.! y | y <- xs]) xs
-}

main = do
  n <- readLn
  mat <- V.replicateM n $ do
    U.unfoldrN n (readInt64 . BS.dropWhile isSpace) <$> BS.getLine
  let -- scoreOneV == U.generate (2^n) (scoreOne n mat)
      scoreOneV :: U.Vector Int64
      scoreOneV = U.create $ do
        vec <- UM.replicate (2^n) minBound
        UM.write vec 0 0
        let go !set = do
              v <- UM.read vec set
              if v == minBound
                then do let !i = countTrailingZeros set
                        let !set' = clearBit set i
                        v0 <- go set'
                        let !v = v0 + sum [(mat V.! i) U.! j | j <- [i+1..n-1], testBit set' j]
                        UM.write vec set v
                        return v
                else return v
        mapM_ go [1..2^n-1]
        return vec
  let result :: Int64
      result = runST $ do
        vec <- UM.replicate (2^n) minBound
        UM.write vec 0 0
        forM_ [0..n-1] $ \i -> do
          UM.write vec (bit i) 0
        let go !set = do
              v <- UM.read vec set
              if v == minBound
                then do let v0 = scoreOneV U.! set
                        let x0:xs = [i | i <- [0..n-1], testBit set i]
                        v <- foldM (\x a -> max x <$> a) v0 [ (+) (scoreOneV U.! set') <$> (go (set `xor` set'))
                                                            | set' <- map (foldl' (.|.) (bit x0)) $ sequence $ map (\i -> [0,bit i]) xs
                                                            , set' /= set
                                                            ]
                        UM.write vec set v
                        return v
                else return v
        go (2^n-1)
  print result

readInt64 :: BS.ByteString -> Maybe (Int64, BS.ByteString)
readInt64 s = first fromIntegral <$> BS.readInt s
