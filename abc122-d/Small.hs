module Small where
import Data.Int
import Data.List
import Data.Array.Unboxed
import Data.Ix (range)

modulo :: Int64
modulo = 10^9 + 7
addMod, subMod, mulMod :: Int64 -> Int64 -> Int64
addMod x y = (x + y) `rem` modulo
subMod x y = (x - y) `mod` modulo
mulMod x y = (x * y) `rem` modulo
sumMod :: [Int64] -> Int64
sumMod = foldl' addMod 0

data Base = A | C | G | T deriving (Eq,Show,Ord,Enum,Ix)

bases :: [Base]
bases = [A,C,G,T]

baseTripleInterval :: ((Base,Base,Base),(Base,Base,Base))
baseTripleInterval = ((A,A,A),(T,T,T))

baseTripleRange :: [(Base,Base,Base)]
baseTripleRange = range baseTripleInterval

sumArr :: UArray (Base,Base,Base) Int64 -> Int64
sumArr = sumMod . elems

-- 出力 a:
--   a ! (s,t,u) : 問題文の条件を満たす長さ 3 の文字列のうち、stuから始まるもの
data3 :: UArray (Base,Base,Base) Int64
data3 = array baseTripleInterval
        [ (i,val)
        | i <- baseTripleRange
        , let val | i `elem` [(A,G,C),(G,A,C),(A,C,G)] = 0
                  | otherwise = 1
        ]

-- sumArr data3 == 61

-- 入力 a:
--   a ! (s,t,u) : 問題文の条件を満たす長さ k の文字列のうち、stuから始まるもの
-- 出力 a':
--   a' ! (s,t,u) : 問題文の条件を満たす長さ k+1 の文字列のうち、stuから始まるもの
step :: UArray (Base,Base,Base) Int64 -> UArray (Base,Base,Base) Int64
step arr = let n = sumArr arr -- 問題文の条件を満たす長さ k の文字列の個数
           in array baseTripleInterval
              [ (i,val)
              | i@(s,t,u) <- baseTripleRange
              , let val | i `elem` [(A,G,C),(G,A,C),(A,C,G)] = 0
                        -- 4文字のダメなパターンで、3文字パターンを含まないもの：ATGC, AGTC, AGGC
                        | i == (A,T,G) = sumMod [arr ! (t,u,v) | v <- [A,G,T]]
                        | i == (A,G,T) = sumMod [arr ! (t,u,v) | v <- [A,G,T]]
                        | i == (A,G,G) = sumMod [arr ! (t,u,v) | v <- [A,G,T]]
                        | otherwise = sumMod [arr ! (t,u,v) | v <- [A,C,G,T]]
              ]

main = do
  n <- readLn
  print $ sumArr $ iterate step data3 !! (n - 3)
