-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr, foldl')
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  [n,k] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ sum $ do xs <- replicateM n [1..k]
                   pure $ fromIntegral $ foldl' gcd 0 xs

modulus :: Int64
modulus = 1_000_000_007

newtype IntMod = IntMod { getIntMod :: Int64 }
  deriving Eq
  deriving newtype Show

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
