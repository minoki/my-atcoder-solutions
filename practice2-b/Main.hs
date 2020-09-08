-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE ConstraintKinds #-}
import           Control.Monad
import           Control.Monad.Primitive      (PrimMonad, PrimState)
import           Data.Bits
import qualified Data.ByteString.Char8        as BS
import           Data.Char                    (isSpace)
import           Data.Int                     (Int64)
import           Data.List                    (unfoldr)
import           Data.Monoid
import qualified Data.Vector.Generic          as G
import qualified Data.Vector.Generic.Mutable  as GM
import qualified Data.Vector.Unboxing         as U
import qualified Data.Vector.Unboxing.Mutable as UM

main = do
  [n,q] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  xs0 <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  tree <- fromVector_BIT (U.coerceVector xs0 :: U.Vector (Sum Int))
  replicateM_ q $ do
    [t,u,v] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    if t == 0 then
      let (p,x) = (u,v) in
      add_BIT tree p (Sum x)
    else do
      let (l,r) = (u,v)
      Sum a <- queryM_BIT tree l
      Sum b <- queryM_BIT tree r
      print $ b - a

--
-- Binary Indexed Tree (BIT), or Fenwick Tree
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
{-# INLINE queryM_BIT #-}

-- index: zero-based
-- property: forall vec i x. do { tree <- fromVector_BIT vec; add_BIT tree i x; return tree } == fromVector_BIT (G.accum (<>) vec [(i,x)])
add_BIT :: (CommutativeMonoid a, GM.MVector mvec a, PrimMonad m) => BIT mvec (PrimState m) a -> Int -> a -> m ()
add_BIT (BIT vec) !i !y = loop (i + 1)
  where
    loop !k | k > GM.length vec = return ()
    loop !k = do x <- GM.read vec (k - 1)
                 GM.write vec (k - 1) $! x <> y
                 loop (k + (k .&. (-k)))
{-# INLINE add_BIT #-}

new_BIT :: (Monoid a, GM.MVector mvec a, PrimMonad m) => Int -> m (BIT mvec (PrimState m) a)
new_BIT n = BIT <$> GM.replicate n mempty

asUnboxedBIT :: (PrimMonad m) => m (BIT UM.MVector (PrimState m) a) -> m (BIT UM.MVector (PrimState m) a)
asUnboxedBIT = id

-- TODO: Efficient initialization
fromVector_BIT :: (CommutativeMonoid a, PrimMonad m, G.Vector vec a) => vec a -> m (BIT (G.Mutable vec) (PrimState m) a)
fromVector_BIT vec = do
  mvec <- GM.replicate (G.length vec) mempty
  G.imapM_ (add_BIT (BIT mvec)) vec
  return (BIT mvec)
{-# INLINE fromVector_BIT #-}
