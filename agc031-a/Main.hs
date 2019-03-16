import Data.Int
import Data.Char
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad

mulMod :: Int64 -> Int64 -> Int64
mulMod a b = a * b `rem` (10^9 + 7)

main :: IO ()
main = do
  n <- readLn :: IO Int -- 1 <= n <= 100000
  s <- getLine -- length s == n
  let vec = V.create $ do
        vec <- VM.replicate 26 1
        forM_ s $ \c -> do
          VM.modify vec (+ 1) (ord c - ord 'a')
        return vec
  print $ (V.foldl' mulMod 1 vec - 1) `mod` (10^9 + 7)
