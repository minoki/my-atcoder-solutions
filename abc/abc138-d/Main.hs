-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr, intersperse)
import Data.Monoid
import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import System.IO (stdout)

main = do
  [n,q] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM (n-1) $ do
    [a,b] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (a-1,b-1)
  operations <- U.replicateM q $ do
    [p,x] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (p-1,x)
  let neighbors :: V.Vector [Int]
      neighbors = V.create $ do
        vec <- VM.replicate n []
        U.forM_ edges $ \(a,b) -> do
          VM.modify vec (b:) a
          VM.modify vec (a:) b
        return vec
      values :: U.Vector Int
      values = U.create $ do
        vec <- UM.replicate n 0
        U.forM_ operations $ \(p,x) -> do
          UM.modify vec (+ x) p
        return vec
      result :: U.Vector Int
      result = U.create $ do
        vec <- UM.new n
        let dfs !parent !q !acc = do
              let !acc' = acc + values U.! q
              UM.write vec q acc'
              forM_ (neighbors V.! q) $ \i -> do
                when (i /= parent) $ do
                  dfs q i acc'
        dfs (-1) 0 0
        return vec
  BSB.hPutBuilder stdout $ (mconcat $ intersperse (BSB.char7 ' ') $ map BSB.intDec $ U.toList result) <> BSB.char7 '\n'
