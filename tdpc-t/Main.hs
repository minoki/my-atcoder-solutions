-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE NoStarIsType  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isSpace)
import           Data.Int              (Int64)
import           Data.List             (tails, unfoldr)
import qualified Data.Vector.Unboxing  as U
import           GHC.TypeNats          (type (+), KnownNat, Nat, type (^),
                                        natVal)

type Poly = U.Vector (IntMod (10^9 + 7))

-- 多項式は
--   U.fromList [a,b,c,...,z] = a + b * X + c * X^2 + ... + z * X^(k-1)
-- により表す。

reduce :: Int -> Poly -> Poly
reduce !k !v | U.last v == 0 = U.init v
             | U.length v <= k = v
             | otherwise = let b = U.last v
                               l = U.length v
                           in reduce k (U.imap (\i a -> if i >= l - k - 1 then a + b else a) (U.init v))

-- 多項式の積を X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
mulP :: Int -> Poly -> Poly -> Poly
mulP !k !v !w = reduce k $ U.generate (U.length v + U.length w - 1) $
                \i -> sum [(v U.! (i-j)) * (w U.! j) | j <- [0..U.length w-1], j <= i, j > i - U.length v]

-- 多項式に X をかけたものを X^k - X^(k-1) - ... - X - 1 で割った余りを返す。
mulByX :: Int -> Poly -> Poly
mulByX !k !v
  | U.length v == k = let !v_k = v U.! (k-1)
                      in U.generate k $ \i -> if i == 0 then
                                                v_k
                                              else
                                                v_k + (v U.! (i - 1))
  | otherwise = U.cons 0 v

-- X の（mod X^k - X^(k-1) - ... - X - 1 での）n 乗
powX :: Int -> Int -> Poly
powX !k !n = doPowX n
  where
    doPowX 0 = U.fromList [1]   -- 1
    doPowX 1 = U.fromList [0,1] -- X
    doPowX i = case i `quotRem` 2 of
                 (j,0) -> let !f = doPowX j -- X^j mod P
                          in mulP k f f
                 (j,_) -> let !f = doPowX j -- X^j mod P
                          in mulByX k (mulP k f f)

main :: IO ()
main = do
  [k,n] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  if n <= k then
    print 1
  else do
    let f = powX k (n - k) -- X^(n-k) mod X^k - X^(k-1) - ... - X - 1
    let seq = replicate k 1 ++ map (sum . take k) (tails seq) -- 数列
    print $ sum $ zipWith (*) (U.toList f) (drop (k-1) seq)

--
-- Modular Arithmetic
--

newtype IntMod (m :: Nat) = IntMod { unwrapN :: Int64 } deriving (Eq)

instance Show (IntMod m) where
  show (IntMod x) = show x

instance KnownNat m => Num (IntMod m) where
  t@(IntMod x) + IntMod y
    | x + y >= modulus = IntMod (x + y - modulus)
    | otherwise = IntMod (x + y)
    where modulus = fromIntegral (natVal t)
  t@(IntMod x) - IntMod y
    | x >= y = IntMod (x - y)
    | otherwise = IntMod (x - y + modulus)
    where modulus = fromIntegral (natVal t)
  t@(IntMod x) * IntMod y = IntMod ((x * y) `rem` modulus)
    where modulus = fromIntegral (natVal t)
  fromInteger n = let result = IntMod (fromInteger (n `mod` fromIntegral modulus))
                      modulus = natVal result
                  in result
  abs = undefined; signum = undefined
  {-# SPECIALIZE instance Num (IntMod 1000000007) #-}

fromIntegral_Int64_IntMod :: KnownNat m => Int64 -> IntMod m
fromIntegral_Int64_IntMod n = result
  where
    result | 0 <= n && n < modulus = IntMod n
           | otherwise = IntMod (n `mod` modulus)
    modulus = fromIntegral (natVal result)

{-# RULES
"fromIntegral/Int->IntMod" fromIntegral = fromIntegral_Int64_IntMod . (fromIntegral :: Int -> Int64) :: Int -> IntMod (10^9 + 7)
"fromIntegral/Int64->IntMod" fromIntegral = fromIntegral_Int64_IntMod :: Int64 -> IntMod (10^9 + 7)
 #-}

instance U.Unboxable (IntMod m) where
  type Rep (IntMod m) = Int64
