-- https://github.com/minoki/my-atcoder-solutions
module Simple where
import Control.Monad (mapM)
import Data.Char (isSpace)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Control.Monad.ST

readIntPair :: BS.ByteString -> (Int, Int)
readIntPair s = let Just (a, s') = BS.readInt s
                    Just (b, _) = BS.readInt $ BS.dropWhile isSpace s'
                in (a, b)

main = do
  (n,m) <- readIntPair <$> BS.getLine
  -- 2 <= n <= 10^5, 1 <= m <= 10^5
  edges <- U.replicateM m $ do
    (x,y) <- readIntPair <$> BS.getLine
    -- 1 <= x, y <= n
    return (x - 1, y - 1 :: Int)
  let -- edges_from ! x : 辺 (x,y) が存在するような y の集合
      edges_from :: V.Vector [Int]
      edges_from = V.create $ do
        sets <- VM.replicate n []
        U.forM_ edges $ \(x,y) -> do
          VM.modify sets (y :) x
        return sets
  let result :: Int
      result = runST $ do
        vec <- UM.replicate n (-1)
        let get x = do
              v <- UM.read vec x
              if v == -1
                then do ys <- mapM get (edges_from V.! x)
                        let v = maximum (0 : map (+1) ys)
                        UM.write vec x v
                        return v
                else return v
        maximum <$> mapM get [0..n-1]
  print result
