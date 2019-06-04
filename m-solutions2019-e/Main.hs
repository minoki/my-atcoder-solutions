-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Char
import Data.Int
import Data.List
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import Data.Monoid
import System.IO
---
import Data.Coerce
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case quotRem f g of
            (q,0) -> (g, u1, v1)
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

modulo = 10^6+3 :: Int32
modulo' = fromIntegral modulo :: Int
addMod, subMod, mulMod, divM :: Int32 -> Int32 -> Int32
addMod !x !y = (x + y) `rem` modulo
subMod !x !y = (x - y) `mod` modulo
mulMod !x !y = fromIntegral ((fromIntegral x * fromIntegral y :: Int64) `rem` fromIntegral modulo)
recipM :: Int32 -> Int32
recipM !x = case exEuclid x modulo of
             (1,a,_) -> a `mod` modulo -- 1 == a * x + _ * modulo, abs a < modulo
             (-1,a,_) -> (-a) `mod` modulo
divM !x !y = x `mulMod` recipM y

newtype N = N { unwrapN :: Int32 } deriving (Eq)
instance Show N where
  show (N x) = show x
instance Num N where
  N x + N y = N ((x + y) `rem` modulo)
  N x - N y = N ((x - y) `mod` modulo)
  N x * N y = N (mulMod x y)
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined

instance Fractional N where
  N x / N y = N (divM x y)
  recip (N x) = N (recipM x)
  fromRational = undefined

factV :: U.Vector N
factV = U.scanl' (*) 1 (U.enumFromN 1 (modulo' - 1))

factM :: Int -> N
factM n | n < modulo' = factV U.! n
        | otherwise = 0

invFactM :: Int -> N
invFactM n | n < modulo' = if even n
                           then - factV U.! (modulo' - n - 1)
                           else factV U.! (modulo' - n - 1)
           | otherwise = error "invFactM: divide by zero"

solve :: Int -> Int -> Int -> N
solve x 0 n = (fromIntegral x)^n
solve !x !d !n = let !x' = fromIntegral x :: N
                     !d' = fromIntegral d :: N
                     !xd = fromIntegral $ unwrapN (x' / d') :: Int
                 in if xd + n - 1 >= modulo'
                    then 0
                    else d'^n * factM (xd + fromIntegral n - 1) * invFactM (xd - 1)

readInt3 :: BS.ByteString -> (Int, Int, Int)
readInt3 s =
    let Just (x, s') = BS.readInt s
        Just (d, s'') = BS.readInt $ BS.dropWhile isSpace s'
        Just (n, _) = BS.readInt $ BS.dropWhile isSpace s''
    in (x, d, n)

readInt3' :: BS.ByteString -> Maybe ((Int, Int, Int), BS.ByteString)
readInt3' s = do
    (x, s') <- BS.readInt $ BS.dropWhile isSpace s
    (d, s'') <- BS.readInt $ BS.dropWhile isSpace s'
    (n, rest) <- BS.readInt $ BS.dropWhile isSpace s''
    return ((x, d, n), rest)

readAndSolve :: BS.ByteString -> Maybe (N, BS.ByteString)
readAndSolve s = do
    (x, s') <- BS.readInt $ BS.dropWhile isSpace s
    (d, s'') <- BS.readInt $ BS.dropWhile isSpace s'
    (n, rest) <- BS.readInt $ BS.dropWhile isSpace s''
    let !result = solve x d n
    return (result, rest)

main = do
  q <- readLn
  -- input <- U.unfoldrN q readInt3' <$> BS.getContents
  -- input <- U.replicateM q (readInt3 <$> BS.getLine)
  -- let answer = U.map (\(x,d,n) -> solve x d n) input
  -- U.mapM_ print answer
  answer <- U.unfoldrN q readAndSolve <$> BS.getContents
  BSB.hPutBuilder stdout $ U.foldr (\x b -> BSB.int32Dec (unwrapN x) <> BSB.char7 '\n' <> b) mempty answer
  {-
  replicateM_ q $ do
    [x,d,n] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    print (solve x d n)
  -}

---

newtype instance UM.MVector s N = MV_N (UM.MVector s Int32)
newtype instance U.Vector N = V_N (U.Vector Int32)

instance Data.Vector.Generic.Mutable.MVector UM.MVector N where -- needs MultiParamTypeClasses here
  basicLength (MV_N mv) = Data.Vector.Generic.Mutable.basicLength mv
  basicUnsafeSlice i l (MV_N mv) = MV_N (Data.Vector.Generic.Mutable.basicUnsafeSlice i l mv)
  basicOverlaps (MV_N mv) (MV_N mv') = Data.Vector.Generic.Mutable.basicOverlaps mv mv'
  basicUnsafeNew l = MV_N <$> Data.Vector.Generic.Mutable.basicUnsafeNew l
  basicInitialize (MV_N mv) = Data.Vector.Generic.Mutable.basicInitialize mv
  basicUnsafeReplicate i x = MV_N <$> Data.Vector.Generic.Mutable.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_N mv) i = coerce <$> Data.Vector.Generic.Mutable.basicUnsafeRead mv i
  basicUnsafeWrite (MV_N mv) i x = Data.Vector.Generic.Mutable.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_N mv) = Data.Vector.Generic.Mutable.basicClear mv
  basicSet (MV_N mv) x = Data.Vector.Generic.Mutable.basicSet mv (coerce x)
  basicUnsafeCopy (MV_N mv) (MV_N mv') = Data.Vector.Generic.Mutable.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_N mv) (MV_N mv') = Data.Vector.Generic.Mutable.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_N mv) n = MV_N <$> Data.Vector.Generic.Mutable.basicUnsafeGrow mv n

instance Data.Vector.Generic.Vector U.Vector N where -- needs MultiParamTypeClasses here
  basicUnsafeFreeze (MV_N mv) = V_N <$> Data.Vector.Generic.basicUnsafeFreeze mv
  basicUnsafeThaw (V_N v) = MV_N <$> Data.Vector.Generic.basicUnsafeThaw v
  basicLength (V_N v) = Data.Vector.Generic.basicLength v
  basicUnsafeSlice i l (V_N v) = V_N (Data.Vector.Generic.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_N v) i = coerce <$> Data.Vector.Generic.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_N mv) (V_N v) = Data.Vector.Generic.basicUnsafeCopy mv v
  elemseq (V_N v) x y = Data.Vector.Generic.elemseq v (coerce x) y

instance U.Unbox N
