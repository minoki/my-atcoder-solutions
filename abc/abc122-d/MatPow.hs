{-# LANGUAGE BangPatterns #-}
module MatPow where
import Data.Array.Unboxed
import Data.Ix (range)
import Data.Int (Int64)
import Data.List (foldl')

modulo = 1000000007 :: Int64
addMod x y = (x + y) `rem` modulo
mulMod x y = (x * y) `rem` modulo
sumMod = foldl' addMod 0
fromIntegerMod :: Integer -> Int64
fromIntegerMod x = fromInteger (x `mod` fromIntegral modulo)

---

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

matPow :: Int -> UArray (Int,Int) Int64 -> Int -> UArray (Int,Int) Int64
matPow k m 0 = array ((1,1),(k,k)) $
               [((i,j), if i == j then 1 else 0) | i <- [1..k], j <- [1..k]]
matPow _ m i = loop (i-1) m m
  where
    loop 0 !_ acc = acc
    loop 1 m acc = m `matMul` acc
    loop i m acc = case i `quotRem` 2 of
                     (j,0) -> loop j (m `matMul` m) acc
                     (j,_) -> loop j (m `matMul` m) (acc `matMul` m)

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
-- characteristic polynomial: -x^7+4*x^6-2*x^4-6*x^3+x^2+x+3

main :: IO ()
main = do
  n <- readLn
  let m = matPow 7 mat n
  print $ m ! (1,1)
