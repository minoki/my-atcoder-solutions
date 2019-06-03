-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
import Data.Char (isSpace)
import Data.Bits (xor)
import Control.Monad (forM_,when)
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IntSet
---
import Data.Coerce
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable

type NN = N
type Vec = U.Vector NN
{-
type NN = Rational
type Vec = V.Vector NN
-}

buildIxMap :: U.Vector Int -> (U.Vector Int, V.Vector IntSet.IntSet)
buildIxMap xs =
  let !classify = V.create $ do
        vec <- VM.replicate 1000 IntSet.empty
        flip U.imapM_ xs $ \i x -> do
          -- 1 <= x <= 1000
          VM.modify vec (IntSet.insert i) (x - 1)
        return vec
      !classify' = V.filter (not . IntSet.null) classify
      !ixToReducedIx = U.create $ do
        vec <- UM.new (U.length xs)
        flip V.imapM_ classify' $ \j s -> do
          forM_ (IntSet.toList s) $ \i -> do
            UM.write vec i j
        return vec
  in (ixToReducedIx, classify')

main = do
  n :: Int <- readLn -- 1 <= n <= 18
  as <- U.unfoldrN (2^n) (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let s = U.sum as :: Int
      !as' = G.map fromIntegral (V.convert as) :: Vec
      !s' = recip (fromIntegral s) :: NN
      toReducedIx :: U.Vector Int
      fromReducedIx :: V.Vector IntSet.IntSet
      (toReducedIx, fromReducedIx) = buildIxMap (U.tail as)
      m = V.length fromReducedIx
      coeffMatV :: V.Vector Vec
      coeffMatV = V.generate m $ \l -> -- 0 <= l < m
        let i = IntSet.findMin (fromReducedIx V.! l) -- 0 <= i < U.length as - 1 = 2^n-1
        in G.generate (m + 1) $ \k ->
          if k == m
          then 1
          else -- 0 <= k < m
            if l == k
            then 1 - s' * sum [ as' G.! ((i+1) `xor` (j+1))
                              | j <- IntSet.toList (fromReducedIx V.! k)
                              -- toReducedIx ! j == l
                              -- 0 <= j < U.length as - 1 = 2^n-1
                              ]
            else - s' * sum [ as' G.! ((i+1) `xor` (j+1))
                            | j <- IntSet.toList (fromReducedIx V.! k)
                            -- toReducedIx ! j == l
                            -- 0 <= j < U.length as - 1 = 2^n-1
                            ]
  let resultV = solveV coeffMatV
  print 0
  U.forM_ toReducedIx $ \i -> do
    let row = resultV V.! i
    print $ row G.! m

-- Gaussian elimination
solveV :: forall vector k. (Eq k, Fractional k, G.Vector vector k) => V.Vector (vector k) -> V.Vector (vector k)
solveV m = runST $ do
  m' <- V.mapM G.thaw m
  m'' <- V.thaw m'
  elim 0 m''
  subst (n-2) m''
  m''' <- V.unsafeFreeze m''
  V.mapM G.unsafeFreeze m'''
  -- V.createT is not available on AtCoder😢
  where
    !n = V.length m
    elim :: Int -> VM.MVector s (G.Mutable vector s k) -> ST s ()
    elim !i !m
      | i >= n = return ()
      | otherwise = do
          let findK k | k >= n = error "singular matrix"
                      | otherwise = do
                          row <- VM.read m k
                          w <- GM.read row i
                          if w == 0
                            then findK (k + 1)
                            else return (k,row,w)
          (k,rowK,w) <- findK i -- i <= k
          let !r = recip w -- r == recip (m!(k,i))
          forM_ [i..GM.length rowK - 1] $ \j -> do
            GM.modify rowK (\x -> x * r) j
          -- GM.set (GM.take i rowK) 0
          forM_ [i..n-1] $ \i' -> do
            row' <- VM.read m i'
            when (i' /= k) $ do
              y <- GM.read row' i -- m!(i',i)
              let !yy = y
              forM_ [i..GM.length row' - 1] $ \j -> do
                z <- GM.read rowK j
                GM.modify row' (\x -> x - z * yy) j
          VM.swap m i k
          elim (i+1) m
    subst :: Int -> VM.MVector s (G.Mutable vector s k) -> ST s ()
    subst !i !m
      | i < 0 = return ()
      | otherwise = do
          row <- VM.read m i
          let loop !j !x | j >= n = GM.write row n x
                         | otherwise = do
                             c <- GM.read row j
                             y <- VM.read m j >>= (`GM.read` n)
                             loop (j+1) (x - c * y)
          rhs <- GM.read row n
          loop (i + 1) rhs
          -- GM.set (GM.drop (i+1) $ GM.take n row) 0
          subst (i - 1) m

---

exEuclid :: (Eq a, Integral a) => a -> a -> (a, a, a)
exEuclid !f !g = loop 1 0 0 1 f g
  where loop !u0 !u1 !v0 !v1 !f 0 = (f, u0, v0)
        loop !u0 !u1 !v0 !v1 !f g =
          case divMod f g of
            (q,r) -> loop u1 (u0 - q * u1) v1 (v0 - q * v1) g r

modulo = 998244353 :: Int
addMod, subMod, mulMod, divM :: Int -> Int -> Int
addMod !x !y = (x + y) `rem` modulo
subMod !x !y = (x - y) `mod` modulo
mulMod !x !y = (x * y) `rem` modulo
recipM :: Int -> Int
recipM !x = case exEuclid x modulo of
             (1,a,_) -> a `mod` modulo
             (-1,a,_) -> (-a) `mod` modulo
divM !x !y = x `mulMod` recipM y

newtype N = N { unwrapN :: Int } deriving (Eq)
instance Show N where
  show (N x) = show x
instance Num N where
  N x + N y = N ((x + y) `rem` modulo)
  N x - N y = N ((x - y) `mod` modulo)
  N x * N y = N ((x * y) `rem` modulo)
  fromInteger n = N (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined

instance Fractional N where
  N x / N y = N (divM x y)
  recip (N x) = N (recipM x)
  fromRational = undefined

---

newtype instance UM.MVector s N = MV_N (UM.MVector s Int)
newtype instance U.Vector N = V_N (U.Vector Int)

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
