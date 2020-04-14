-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector.Unboxing as U
import qualified Data.Vector.Unboxing.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  [n,k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let v :: U.Vector IntMod
      v = U.create $ do
        v <- UM.replicate (k+1) 0
        forM_ [1..k] $ \i -> do
          UM.write v i $! fromIntegral (k `quot` i) ^ n
        return v
      w :: U.Vector IntMod
      w = U.create $ do
        w <- U.thaw v
        forM_ [k,k-1..1] $ \i -> do
          s <- sum <$> sequence [ UM.read w j | j <- [2*i,3*i..k] ]
          UM.modify w (subtract s) i
        return w
  print $ sum [ fromIntegral i * w U.! i | i <- [1..k] ]

modulus :: Int64
modulus = 10^9 + 7

newtype IntMod = IntMod { getIntMod :: Int64 } deriving Eq

instance Show IntMod where
  show (IntMod x) = show x

instance Num IntMod where
  IntMod x + IntMod y = IntMod ((x + y) `rem` modulus)
  IntMod x - IntMod y = IntMod ((x - y) `mod` modulus)
  IntMod x * IntMod y = IntMod ((x * y) `rem` modulus)
  negate (IntMod x) = IntMod (negate x `mod` modulus)
  fromInteger x = IntMod (fromInteger (x `mod` fromIntegral modulus))
  abs = undefined; signum = undefined

{-# RULES
"fromIntegral/Int64->IntMod" forall (x :: Int64).
  fromIntegral x = IntMod (x `mod` modulus)
"fromIntegral/Int->IntMod" forall (x :: Int).
  fromIntegral x = IntMod (fromIntegral x `mod` modulus)
  #-}

instance U.Unboxable IntMod where
  type Rep IntMod = Int64
