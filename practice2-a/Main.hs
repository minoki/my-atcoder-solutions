-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.ByteString.Builder     as BSB
import qualified Data.ByteString.Char8       as BS
import           Data.Char                   (isSpace)
import           Data.Foldable               (foldlM)
import           Data.List                   (unfoldr)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           System.IO                   (stdout)

-- Union Find
newUnionFindTree :: PrimMonad m => Int -> m (UM.MVector (PrimState m) Int)
newUnionFindTree n = U.thaw $ U.enumFromN 0 n

getRoot :: PrimMonad m => UM.MVector (PrimState m) Int -> Int -> m Int
getRoot !v = go
  where
    go !i = do
      j <- UM.read v i
      if i == j then
        return i
      else do
        k <- go j
        UM.write v i k
        return k
{-# INLINE getRoot #-}

unify :: PrimMonad m => UM.MVector (PrimState m) Int -> Int -> Int -> m ()
unify !v !i !j = do
  i' <- getRoot v i
  j' <- getRoot v j
  unless (i' == j') $ do
    let !k = min i' j'
    UM.write v i' k
    UM.write v j' k
{-# INLINE unify #-}

main = do
  [n,q] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  uf <- newUnionFindTree n
  {-
  queries <- U.replicateM q $ do
    [t,u,v] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (t,u,v)
  bb <- U.foldM (\bb (t,u,v) ->
                   if t == 0 then
                     unify uf u v >> return bb
                   else do
                     i <- getRoot uf u
                     j <- getRoot uf v
                     return $! bb <> if i == j then BSB.string8 "1\n" else BSB.string8 "0\n"
                ) mempty queries
  BSB.hPutBuilder stdout bb
  -}
  replicateM_ q $ do
    [t,u,v] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    if t == 0 then
      unify uf u v
    else do
      i <- getRoot uf u
      j <- getRoot uf v
      putStrLn $ if i == j then "1" else "0"
