-- https://github.com/minoki/my-atcoder-solutions
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  s <- BS.getLine
  let n = BS.length s
  let result :: U.Vector (Int, Int)
      result = U.create $ do
        v <- UM.new (n+1)
        UM.write v 0 (0, 0)
        UM.write v 1 (1, 0)
        forM_ [2..n] $ \i -> do
          (a1, b1) <- UM.read v (i-1)
          (a2, b2) <- UM.read v (i-2)
          let a | BS.index s (i-2) == BS.index s (i-1) = b1 + 1
                | otherwise = max a1 b1 + 1
              b | i >= 4 && BS.index s (i-4) == BS.index s (i-2) && BS.index s (i-3) == BS.index s (i-1) = a2 + 1
                | otherwise = max a2 b2 + 1
          UM.write v i (a, b)
        return v
      (a, b) = result U.! n
  print (max a b)
