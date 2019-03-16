{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.List
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

infixl 7 <.>

newtype Perm = Perm (V.Vector Int) deriving Eq

permFromIntList :: Int -> [Int] -> Perm
permFromIntList n xs = Perm $ V.fromListN n $ map (subtract 1) xs

identPerm :: Int -> Perm
identPerm n = Perm $ V.fromListN n [0..n-1]

compPerm :: Perm -> Perm -> Perm
compPerm (Perm f) (Perm g)
  = let n = V.length f -- = V.length g
    in Perm $ V.generate n $ \i -> f V.! (g V.! i)

(<.>) :: Perm -> Perm -> Perm
(<.>) = compPerm

invPerm :: Perm -> Perm
invPerm (Perm p) = Perm $ V.create $ do
  let n = V.length p
  q <- VM.new n
  forM_ [0..n-1] $ \i -> do
    VM.write q (p V.! i) i
  return q

powPerm :: Int -> Perm -> Int -> Perm
powPerm n _ 0 = identPerm n
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
      f p q = q `compPerm` invPerm p

  {-
  let as = p : q : zipWith f as (tail as)
      a_k = as !! (k - 1)
  putStrLn $ showPerm a_k
  -}
  {-
  let loop i p q | i == 0 = p
                 | otherwise = loop (i - 1) q (f p q)
  putStrLn $ showPerm $ loop (k - 1) p q
  -}

  let m = (k + 1) `quot` 2
      r = (q <.> invPerm p) <.> (invPerm q <.> p)
      b = case m `quotRem` 3 of
            (l,0) -> powPerm n r (l-1) <.> q <.> invPerm p
            (l,1) -> powPerm n r l
            (l,_) -> powPerm n r l <.> q
      c = case k `rem` 6 of
            0 -> invPerm q <.> p
            1 -> p
            2 -> q
            3 -> invPerm p <.> q
            4 -> invPerm p
            _ -> invPerm q
      a_k' = b <.> c <.> invPerm b
  putStrLn $ showPerm a_k'
