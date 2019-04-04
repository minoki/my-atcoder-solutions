{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST

modulo :: Int64
modulo = 10^9 + 7
addMod, subMod, mulMod :: Int64 -> Int64 -> Int64
addMod x y = (x + y) `rem` modulo
subMod x y = (x - y) `mod` modulo
mulMod x y = (x * y) `rem` modulo

main = do
  [h,w] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 2 <= h, w <= 1000
  cells :: V.Vector BS.ByteString <- V.replicateM h $ BS.getLine
  let result :: UArray (Int,Int) Int64
      result = runSTUArray $ do
        arr <- newArray ((0,0),(h,w)) 0
        writeArray arr (1,1) 1
        forM_ [1..h] $ \i -> do
          forM_ [1..w] $ \j -> do
            if (i /= 1 || j /= 1) && (cells V.! (i - 1)) `BS.index` (j - 1) == '.'
              then do s <- readArray arr (i-1,j)
                      t <- readArray arr (i,j-1)
                      writeArray arr (i,j) (s `addMod` t)
              else return ()
        return arr
  print $ result ! (h,w)
