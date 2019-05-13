{-# LANGUAGE BangPatterns #-}
import Data.Int
import Data.Char
import Data.Bits
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap

modulo = 10^9+7 :: Int64
addMod, subMod, mulMod :: Int64 -> Int64 -> Int64
addMod !x !y = (x + y) `rem` modulo
subMod !x !y = (x - y) `mod` modulo
mulMod !x !y = (x * y) `rem` modulo

newtype N = N { unwrapN :: Int64 } deriving (Eq)
instance Show N where
  show (N x) = show x
instance Num N where
  N x + N y = N ((x + y) `rem` modulo)
  N x - N y = N ((x - y) `mod` modulo)
  N x * N y = N ((x * y) `rem` modulo)
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined

main = do
  n <- readLn
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let ys = U.scanl xor 0 xs
  let z = U.last ys
  if z /= 0
    then let f ys = let (ys0, ys1) = U.span (== 0) ys
                        (ys', ys'') = U.span (/= 0) ys1
                        l = U.length ys0
                        k = U.length $ U.filter (== z) ys'
                    in if U.null ys''
                       then fromIntegral l
                       else (fromIntegral l * fromIntegral k) * f ys'' + 1
         in print (f ys :: N)
    else let f :: U.Vector Int -> (Int, IntMap.IntMap N, IntMap.IntMap N)
             f ys = let (ys0, ys1) = U.span (== 0) ys
                        (ys', ys'') = U.span (/= 0) ys1
                        l = U.length ys0
                    in if U.null ys1
                       then (l, IntMap.empty, IntMap.empty)
                       else let (l0, s, t) = f ys''
                                tx = IntMap.fromListWith (+) [(a, IntMap.findWithDefault 1 a t) | a <- U.toList ys']
                                s' = IntMap.unionWith (+) s tx
                                t' = IntMap.unionWith (+) (fmap (fromIntegral l *) s') $ IntMap.union t tx
                                !l' = l0 + l
                            in (l', s', t')
             !(ll, ss, tt) = f ys
         in print (2 ^ (ll - 2) + getSum (IntMap.foldMapWithKey (\k v -> Sum v) ss) :: N)
