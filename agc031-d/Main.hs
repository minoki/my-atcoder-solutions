{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.List
import Data.Int
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

newtype Perm = Perm (V.Vector Int16) deriving Eq

permFromIntList :: Int -> [Int] -> Perm
permFromIntList n xs = Perm $ V.fromListN n $ map (subtract 1 . fromIntegral) xs

compPerm :: Perm -> Perm -> Perm
compPerm (Perm f) (Perm g)
  = let n = V.length f -- = V.length g
    in Perm $ V.generate n $ \i -> f V.! (fromIntegral $ g V.! i)

invPerm :: Perm -> Perm
invPerm (Perm p) = Perm $ V.create $ do
  let n = V.length p
  q <- VM.new n
  forM_ [0..n-1] $ \i -> do
    VM.write q (fromIntegral $ p V.! i) (fromIntegral i)
  return q

powPerm :: Int -> Perm -> Int -> Perm
powPerm n _ 0 = Perm $ V.fromListN n [0..fromIntegral (n-1)] -- identity
powPerm _ p i = loop (i-1) p p
  where loop 0 !p !acc = acc
        loop 1 p acc = p `compPerm` acc
        loop i p acc = case quotRem i 2 of
                         (j,0) -> loop j (p `compPerm` p) acc
                         (j,_) -> loop j (p `compPerm` p) (acc `compPerm` p)

showPerm :: Perm -> String
showPerm (Perm p) = intercalate " " $ map (show . (+ 1)) $ V.toList p

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

main = do
  [n,k] <- parseInts <$> getLine
  ps <- parseInts <$> getLine
  qs <- parseInts <$> getLine
  let p = permFromIntList n ps
      q = permFromIntList n qs
      r = (q `compPerm` invPerm p) `compPerm` (invPerm q `compPerm` p)
      f p q = q `compPerm` invPerm p
  let as = p : q : zipWith f as (tail as)
      a_k = as !! (k - 1)
  putStrLn $ showPerm a_k
  {-
  let loop i p q | i == 0 = p
                 | otherwise = loop (i - 1) q (f p q)
  putStrLn $ showPerm $ loop (k - 1) p q
  -}
