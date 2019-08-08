-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.Int (Int64)
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS
import Data.Array.ST
import Control.Monad.ST

asSTUArray :: ST s (STUArray s i e) -> ST s (STUArray s i e)
asSTUArray = id

main = do
  n <- readLn
  xs <- U.map fromIntegral . U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let result = runST $ do
        arr <- asSTUArray $ newArray ((0,0),(n,n)) 0
        -- arr!(i,j): 左から i 個、右から j 個取り除かれた状態から二人が最適に行動した場合の、 X-Y の値
        -- i+j == n の場合は arr!(i,j) == 0
        forM_ [n-1,n-2..0] $ \k ->
          if even k
          then -- 太郎くん： X-Y を最大化したい
            forM_ [0..k] $ \i -> do
              let j = k - i
              -- i + j == k
              a <- (+ xs U.! i) <$> readArray arr (i+1,j)
              b <- (+ xs U.! (n - j - 1)) <$> readArray arr (i,j+1)
              writeArray arr (i,j) $! max a b
          else -- 次郎くん： X-Y を最小化したい
            forM_ [0..k] $ \i -> do
              let j = k - i
              -- i + j == k
              a <- subtract (xs U.! i) <$> readArray arr (i+1,j)
              b <- subtract (xs U.! (n - j - 1)) <$> readArray arr (i,j+1)
              writeArray arr (i,j) $! min a b
        readArray arr (0,0)
  print (result :: Int64)
