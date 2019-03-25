{-# LANGUAGE BangPatterns #-}
module PolyDiv where
import Data.Array.Unboxed
import qualified Data.Vector.Unboxed as V
import Data.Ix (range)
import Data.Int (Int64)
import Data.List (foldl')

modulo = 1000000007 :: Int64
addMod x y = (x + y) `rem` modulo
mulMod x y = (x * y) `rem` modulo
sumMod = foldl' addMod 0
fromIntegerMod :: Integer -> Int64
fromIntegerMod x = fromInteger (x `mod` fromIntegral modulo)

-- 多項式は昇冪の順
--   V.fromList [a,b,c,...,z] = a + b * X + c * X^2 + ... + z * X^(k-1)
-- により表す。
-- 長さは常に7とする。
type Poly = V.Vector Int64

-- 多項式を -X^7+4*X^6-2*X^4-6*X^3+X^2+X+3 で割った余りを返す。
reduce :: V.Vector Int64 -> V.Vector Int64
reduce v | V.length v <= 7 = v
         | V.last v == 0 = reduce $ V.init v
         | otherwise = let b = V.last v
                           l = V.length v - 8
                       in reduce $ V.accum addMod (V.init v) [(l,3 `mulMod` b),(l+1,b),(l+2,b),(l+3,fromIntegerMod (-6) `mulMod` b),(l+4,fromIntegerMod (-2) `mulMod` b),(l+6,4 `mulMod` b)]

-- 多項式の積を -X^7+4*X^6-2*X^4-6*X^3+X^2+X+3 で割った余りを返す。
mulP :: Poly -> Poly -> Poly
mulP v w = reduce $ V.generate (7 + 7 - 1) $
           \i -> sumMod [(v V.! (i-j)) `mulMod` (w V.! j) | j <- [0..7-1], j <= i, j > i - 7]

-- 多項式に X をかけたものを -X^7+4*X^6-2*X^4-6*X^3+X^2+X+3 で割った余りを返す。
mulByX :: Poly -> Poly
mulByX v = let b = V.last v
           in V.accum addMod (V.cons 0 $ V.init v) [(0,3 `mulMod` b),(1,b),(2,b),(3,fromIntegerMod (-6) `mulMod` b),(4,fromIntegerMod (-2) `mulMod` b),(6,4 `mulMod` b)]

-- X の（mod -X^7+4*X^6-2*X^4-6*X^3+X^2+X+3 での）n 乗
powX :: Int -> Poly
powX 0 = V.fromList [1,0,0,0,0,0,0]
powX 1 = V.fromList [0,1,0,0,0,0,0]
powX i = case i `quotRem` 2 of
           (j,0) -> let !f = powX j
                    in mulP f f
           (j,_) -> let !f = powX j
                    in mulByX (mulP f f)

matMul :: UArray (Int,Int) Int64 -> UArray (Int,Int) Int64 -> UArray (Int,Int) Int64
matMul a b = let ((i0,j0),(ix,jx)) = bounds a
                 ((j'0,k0),(j'x,kx)) = bounds b
             in if jx - j0 == j'x - j'0
                   then array ((i0,k0),(ix,kx))
                        [ ((i,k), sumMod [(a!(i,j)) `mulMod` (b!(j',k)) | (j,j') <- zip (range (j0,jx)) (range (j'0,j'x))])
                        | i <- range (i0,ix)
                        , k <- range (k0,kx)
                        ]
                else error "Matrix size mismatch"

matAdd :: UArray (Int,Int) Int64 -> UArray (Int,Int) Int64 -> UArray (Int,Int) Int64
matAdd a b = array (bounds a)
             [ (k, (a ! k) `addMod` (b ! k))
             | k <- range (bounds a)
             ]

mat :: UArray (Int,Int) Int64
mat = array ((1,1),(7,7))
      [ ((i,j),aij)
      | (i,ai) <- zip [1..] values
      , (j,aij) <- zip [1..] ai
      ]
  where values = fmap (fmap fromIntegerMod)
          [[4, 0,-2,-3,-1, 0, 1]
          ,[1, 0, 0, 0, 0, 0, 0]
          ,[0, 1, 0, 0, 0, 0, 0]
          ,[0, 0, 1, 0, 0, 0, 0]
          ,[0, 1,-1, 0, 0, 0, 1]
          ,[0, 0, 0, 0, 1, 0, 0]
          ,[0, 0, 0, 0, 0, 1, 0]
          ]
-- characteristic polynomial: -X^7+4*X^6-2*X^4-6*X^3+X^2+X+3

zeroMat :: UArray (Int,Int) Int64
zeroMat = array ((1,1),(7,7))
          [ ((i,j),0) | i <- [1..7], j <- [1..7]]

scalarMat :: Int64 -> UArray (Int,Int) Int64
scalarMat x = array ((1,1),(7,7))
              [ ((i,j), if i == j then x else 0) | i <- [1..7], j <- [1..7]]

applyPolyToMat :: V.Vector Int64 -> UArray (Int,Int) Int64 -> UArray (Int,Int) Int64
applyPolyToMat f m = V.foldr' (\a b -> (b `matMul` m) `matAdd` scalarMat a) zeroMat f

main :: IO ()
main = do
  n <- readLn
  let f = powX n -- X^n mod -X^7+4*X^6-2*X^4-6*X^3+X^2+X+3
      m = applyPolyToMat f mat -- = matPow mat n
  print $ m ! (1,1)
