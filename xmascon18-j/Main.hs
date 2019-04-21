{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Int
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T (getLine)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

parseSen :: T.Text -> (Int64, T.Text)
parseSen s = doParse 0 s
  where
    doParse :: Int64 -> T.Text -> (Int64, T.Text)
    doParse !acc s = case T.uncons s of
      Just ('〇', xs) -> (acc, xs) -- acc should be 0
      Just ('一', xs) -> parseSenRest acc 1 xs
      Just ('二', xs) -> parseSenRest acc 2 xs
      Just ('三', xs) -> parseSenRest acc 3 xs
      Just ('四', xs) -> parseSenRest acc 4 xs
      Just ('五', xs) -> parseSenRest acc 5 xs
      Just ('六', xs) -> parseSenRest acc 6 xs
      Just ('七', xs) -> parseSenRest acc 7 xs
      Just ('八', xs) -> parseSenRest acc 8 xs
      Just ('九', xs) -> parseSenRest acc 9 xs
      Just ('十', xs) -> doParse (acc + 10) xs
      Just ('百', xs) -> doParse (acc + 100) xs
      Just ('千', xs) -> doParse (acc + 1000) xs
      _ -> (acc, s)
    parseSenRest !acc !i s = case T.uncons s of
      Just ('十', xs) -> doParse (acc + i * 10) xs
      Just ('百', xs) -> doParse (acc + i * 100) xs
      Just ('千', xs) -> doParse (acc + i * 1000) xs
      _ -> doParse (acc + i) s

-- |
-- >>> parseKansuji "三百三十三"
-- (333,"")
-- >>> parseKansuji "二十六万二千百四十四"
-- (262144,"")
-- >>> parseKansuji "十億十"
-- (1000000010,"")
parseKansuji :: T.Text -> (Int64, T.Text)
parseKansuji s = doParse 0 s
  where
    doParse !acc s = let (v, ss) = parseSen s
                     in case T.uncons ss of
                          Just ('万', xs) -> doParse (acc + v * 10000) xs
                          Just ('億', xs) -> doParse (acc + v * 10^8) xs
                          _ -> (acc + v, ss)

intToKansujiDigit :: Int -> Char
intToKansujiDigit !i = "〇一二三四五六七八九" !! i

-- 一万未満の自然数を表示する
showKansujiSen :: Int64 -> String -> String
showKansujiSen 0 s = s
showKansujiSen n s
  | n < 0 || n >= 10000 = error "showKansujiSen: out of range"
  | otherwise = let (sen,n') = n `quotRem` 1000
                    (hyaku,n'') = n' `quotRem` 100
                    (ju,ichi) = n'' `quotRem` 10
                    intToKansujiDigit' 0 suffix s = s
                    intToKansujiDigit' 1 suffix s = suffix : s
                    intToKansujiDigit' n suffix s = intToKansujiDigit (fromIntegral n) !: suffix : s
                in intToKansujiDigit' sen '千'
                   $ intToKansujiDigit' hyaku '百'
                   $ intToKansujiDigit' ju '十'
                   $ (if ichi == 0 then s else intToKansujiDigit (fromIntegral ichi) !: s)

-- |
-- >>> showKansuji 12345 == "一万二千三百四十五"
-- True
-- >>> showKansuji 12345678 == "千二百三十四万五千六百七十八"
-- True
-- >>> showKansuji 123456789 == "一億二千三百四十五万六千七百八十九"
-- True
showKansuji :: Int64 -> String
showKansuji 0 = "〇"
showKansuji n = let (oku,n') = n `quotRem` (10^8)
                    (man,ichi) = n' `quotRem` 10000
                    showKansujiSen' 0 suffix s = s
                    showKansujiSen' n suffix s = showKansujiSen n (suffix : s)
                in showKansujiSen' oku '億'
                   $ showKansujiSen' man '万'
                   $ if ichi == 0 then "" else showKansujiSen ichi ""

data Expr = Lit !Int64
          | Pow Expr Expr
          deriving (Eq,Show)

-- |
-- >>> parseExpr "四の三乗の二乗"
-- (Pow (Pow (Lit 4) (Lit 3)) (Lit 2),"")
-- >>> parseExpr "四の三の二乗乗"
-- (Pow (Lit 4) (Pow (Lit 3) (Lit 2)),"")
-- >>> parseExpr "一億二千三百四十五万六千七百八十九の二の〇の〇乗乗乗"
-- (Pow (Lit 123456789) (Pow (Lit 2) (Pow (Lit 0) (Lit 0))),"")
parseExpr :: T.Text -> (Expr, T.Text)
parseExpr s = let (v, ss) = parseKansuji s
              in parseRest (Lit v) ss
  where
    parseRest e s = case T.uncons s of
      Just ('の', xs) -> let (e2, xss) = parseExpr xs
                         in case T.uncons xss of
                              Just ('乗', xs) -> parseRest (Pow e e2) xs
                              _ -> error $ "Malformed input: " ++ T.unpack (T.take 10 xss)
      _ -> (e, s)

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

recipM :: (Eq a, Integral a, Show a) => a -> a -> a
recipM !x modulo = case exEuclid x modulo of
                     (1,a,_) -> a `mod` modulo
                     (-1,a,_) -> (-a) `mod` modulo
                     (g,a,b) -> error $ show x ++ "^(-1) mod " ++ show modulo ++ " failed: gcd=" ++ show g

-- |
-- >>> crt 3 6 2 7
-- 9
-- >>> crt 2 5 3 9
-- 12
crt :: Int64 -> Int64 -> Int64 -> Int64 -> Int64
crt !a1 !m1 !a2 !m2 = let m1' = recipM m1 m2
                          m2' = recipM m2 m1
                      in (m2 * m2' * a1 + m1 * m1' * a2) `mod` (m1 * m2)

infixr 5 !:
(!:) :: a -> [a] -> [a]
(!x) !: xs = x : xs

sieve :: Int -> [Int64]
sieve !max = 2 : U.ifoldr (\i isPrime xs -> if isPrime then fromIntegral (2 * i + 1) !: xs else xs) [] vec
  where
    vec = U.create $ do
      vec <- UM.replicate ((max - 1) `quot` 2 + 1) True
      UM.write vec 0 False -- 1 is not a prime
      -- vec ! i : is (2 * i + 1) prime?
      let clear !p = forM_ [3*p,5*p..max] $ \n -> UM.write vec (n `quot` 2) False
          factorBound = floor (sqrt (fromIntegral max) :: Double)
          loop !i | 2 * i + 1 > factorBound = return ()
                  | otherwise = do b <- UM.read vec i
                                   when b $ clear (2 * i + 1)
                                   loop (i + 1)
      loop 1
      return vec

-- |
-- >>> takeWhile (< 100) primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
primes :: [Int64]
primes = sieve 31622
-- floor (sqrt (10^9+9)) == 31622
-- length primes == 3401

-- x <= 10^9+9
-- |
-- >>> factor 100
-- [(2,2),(5,2)]
-- >>> factor 144
-- [(2,4),(3,2)]
-- >>> factor (10^9+6)
-- [(2,1),(500000003,1)]
-- >>> factor (10^9+7)
-- [(1000000007,1)]
factor :: Int64 -> [(Int64, Int)]
factor 0 = error "factor 0"
factor x | x > 10^9+9 = error "factor: too large"
factor x = loop x primes
  where
    loop 1 _ = []
    loop x (p:ps) = case factorOut 0 x p of
                      (0,y) -> loop x ps
                      (n,y) -> (p,n) : loop y ps
    loop x [] = [(x,1)]
    factorOut !n !x !p | (q,0) <- x `quotRem` p = factorOut (n+1) q p
                       | otherwise = (n, x)

-- euler n = length [x | x <- [1..n], gcd n x == 1]
-- |
-- >>> euler 324
-- 108
-- >>> euler 144
-- 48
euler :: Int64 -> Int64
euler !x = product [(p - 1) * p^(n-1) | (p,n) <- factor x]

-- |
-- >>> powMod 36 5 107
-- 48
powMod :: Int64 -> Int64 -> Int64 -> Int64
powMod !_ 0 !_ = 1
powMod !a b !modulo = loop a a (b - 1)
  where
    loop :: Int64 -> Int64 -> Int64 -> Int64
    loop !acc !a 0 = acc
    loop !acc !a 1 = (acc * a) `rem` modulo
    loop !acc !a i = case i `quotRem` 2 of
                       (j,0) -> loop acc ((a * a) `rem` modulo) j
                       (j,_) -> loop ((acc * a) `rem` modulo) ((a * a) `rem` modulo) j

isZero :: Expr -> Bool
isZero (Lit x) = x == 0
isZero (Pow base exp) = isZero base && not (isZero exp)

-- powSat base exp upperBound = min upperBound (base ^ exp)
powSat :: Int64 -> Int64 -> Int64 -> Int64
powSat !_ 0 !_ = 1
powSat !0 _ !_ = 0
powSat !a b !upperBound = loop a a (b - 1)
  where
    -- Assumption: a > 0, b > 0, upperBound^2 <= maxBound
    loop :: Int64 -> Int64 -> Int64 -> Int64
    loop !acc !a 0 = min acc upperBound
    loop !acc !a 1 = acc `mul` a
    loop !acc !a i | acc >= upperBound || a >= upperBound = upperBound
    loop !acc !a i = case i `quotRem` 2 of
                       (j,0) -> loop acc (a `mul` a) j
                       (j,_) -> loop (acc `mul` a) (a `mul` a) j
    mul :: Int64 -> Int64 -> Int64
    -- assumption: x > 0, y > 0
    mul x y | x >= upperBound || y >= upperBound = upperBound
            | otherwise = min upperBound (x * y)

upperBound :: Int64
upperBound = 2^31 - 1

-- evalSat expr = min upperBound (eval expr)
evalSat :: Expr -> Int64
evalSat (Lit x) = min x upperBound
evalSat (Pow base exp) = let base' = evalSat base
                             exp' = evalSat exp
                         in case (base', exp') of
                              (_, 0) -> 1
                              (0, _) -> 0
                              (1, _) -> 1
                              -- now x >= 2
                              (x, y) | y >= 32 || x >= upperBound -> upperBound
                                     | otherwise -> powSat x y upperBound

-- (p,q) = splitCoprime x y
-- gcd p q == 1, gcd p y == 1
-- |
-- >>> splitCoprime 72 2
-- (9,8)
-- >>> splitCoprime 72 3
-- (8,9)
splitCoprime :: Int64 -> Int64 -> (Int64, Int64)
splitCoprime !x !y = loop x 1
  where
    loop !p !q = case gcd p y of
                   1 -> (p, q)
                   p' -> loop (p `quot` p') (q * p')

evalMod :: Expr -> Int64 -> Int64
evalMod _ 1 = 0
evalMod (Lit x) modulo = x `rem` modulo
evalMod (Pow base exp) modulo
  | isZero exp = 1
  | isZero base = 0
  | otherwise = let base' = evalMod base modulo
                    (m1,m2) = splitCoprime modulo base'
                    -- m1 * m2 == modulo, gcd m1 m2 == 1, gcd m1 base' == 1
                    -- m1 = euler g'
                    exp' = evalMod exp (euler m1)
                    a1 = powMod base' exp' m1
                    a2 | m2 == 1 = 0 -- base' ^ exp mod m2
                       | otherwise = let m2f = factor m2
                                         m2n = maximum (map snd m2f)
                                         exp'' = evalSat exp
                                     in if exp' >= fromIntegral m2n || exp'' >= fromIntegral m2n
                                        then 0
                                        else powMod base' exp'' m2
                in crt a1 m1 a2 m2

solve :: T.Text -> Int64
solve s = case parseExpr s of
            (expr, "") -> evalMod expr (10^9+9)

main = do
  s <- T.getLine
  let result = solve s
  putStrLn $ showKansuji result

-- $setup
-- >>> :set -XOverloadedStrings
