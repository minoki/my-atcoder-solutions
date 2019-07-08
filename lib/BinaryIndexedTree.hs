{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
module BinaryIndexedTree where
import Data.Monoid
import Data.Bits
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.Primitive
-- For testing:
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as UM

--
-- Binary Indexed Tree (BIT)
--

type CommutativeMonoid a = Monoid a

newtype BIT mvec s a = BIT (mvec s a)

-- index: 1-based
-- property: forall vec i. fromVector_BIT vec >>= flip queryM_BIT i == pure (G.scanl (<>) mempty vec G.! i)
queryM_BIT :: (Monoid a, GM.MVector mvec a, PrimMonad m) => BIT mvec (PrimState m) a -> Int -> m a
queryM_BIT (BIT vec) !i = doQuery i mempty
  where
    doQuery 0 !acc = return acc
    doQuery i !acc = do y <- GM.read vec (i - 1)
                        let !j = (i - 1) .&. i
                        doQuery j (y <> acc)

-- index: zero-based
-- property: forall vec i x. do { tree <- fromVector_BIT vec; add_BIT tree i x; return tree } == fromVector_BIT (G.accum (<>) vec [(i,x)])
add_BIT :: (CommutativeMonoid a, GM.MVector mvec a, PrimMonad m) => BIT mvec (PrimState m) a -> Int -> a -> m ()
add_BIT (BIT vec) !i !y = loop (i + 1)
  where
    loop !k | k > GM.length vec = return ()
    loop !k = do x <- GM.read vec (k - 1)
                 GM.write vec (k - 1) $! x <> y
                 loop (k + (k .&. (-k)))

new_BIT :: (Monoid a, GM.MVector mvec a, PrimMonad m) => Int -> m (BIT mvec (PrimState m) a)
new_BIT n = BIT <$> GM.replicate n mempty

asBoxedBIT :: (PrimMonad m) => m (BIT VM.MVector (PrimState m) a) -> m (BIT VM.MVector (PrimState m) a)
asBoxedBIT = id

asUnboxedBIT :: (PrimMonad m) => m (BIT UM.MVector (PrimState m) a) -> m (BIT UM.MVector (PrimState m) a)
asUnboxedBIT = id

--
-- Tests
--

fromVector_BIT :: (CommutativeMonoid a, PrimMonad m, G.Vector vec a) => vec a -> m (BIT (G.Mutable vec) (PrimState m) a)
fromVector_BIT vec = do
  mvec <- GM.replicate (G.length vec) mempty
  G.imapM_ (add_BIT (BIT mvec)) vec
  return (BIT mvec)

toVector_BIT :: (Monoid a, PrimMonad m, G.Vector vec a) => BIT (G.Mutable vec) (PrimState m) a -> m (vec a)
toVector_BIT tree@(BIT mvec) = G.generateM (GM.length mvec + 1) (queryM_BIT tree)

test :: (Eq a, Monoid a, G.Vector vec a) => vec a -> IO Bool
test vec = do
  tree <- fromVector_BIT vec
  acc <- toVector_BIT tree
  return (acc `G.eq` G.scanl (<>) mempty vec)

test1 :: IO Bool
test1 = test (V.fromList [Sum 1, Sum 4, Sum (-1), Sum 5, Sum 7, Sum 0, Sum (-2)])

{-
test2 :: IO Bool
test2 = test (V.fromList ["H", "e", "ll", "o", "w", "o", "rl", "d", "!"])
-}
