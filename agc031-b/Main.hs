import Control.Monad
import Data.Int
import Data.List
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

modulo :: Int64
modulo = 10^9 + 7

addMod a b = (a + b) `rem` modulo
subMod a b = (a - b) `mod` modulo
mulMod a b = (a * b) `mod` modulo

-- >>> makeIndex $ V.fromList [1,2,1,2,2]
-- [2,3,-1,4,-1]
-- >>> makeIndex $ V.fromList [4,2,5,4,2,4]
-- [3,4,-1,5,-1,-1]
-- >>> makeIndex $ V.fromList [1,3,1,2,3,3,2]
-- [2,4,-1,6,5,-1,-1]
makeIndex :: V.Vector Int -> V.Vector Int
makeIndex v = V.create $ do
  let n = V.length v
  w <- VM.new n
  idx <- VM.replicate (2*10^5+1) (-1)
  forM_ [n-1,n-2..0] $ \i -> do
    let c = v V.! i
    j <- VM.read idx c
    VM.write w i j
    VM.write idx c i
  return w

build :: V.Vector Int -> V.Vector Int -> V.Vector Int64
build v idx = V.create $ do
  let n = V.length v
  result <- VM.new n
  VM.write result (n-1) 1
  forM_ [n-2,n-3..0] $ \i -> do
    let c = v V.! i
        d = v V.! (i + 1)
    k <- VM.read result (i + 1)
    if c == d
      then VM.write result i k -- same color
      else do let j = idx V.! i
              if j /= -1
                then do u <- VM.read result j
                        VM.write result i (addMod k u)
                else VM.write result i k
  return result

main = do
  n <- readLn -- 1 <= n <= 2*10^5
  cs <- V.fromListN n <$> (sequence $ replicate n readLn) -- 1 <= ci <= 2*10^5
  let idx = makeIndex cs
      result = build cs idx
  print (V.head result)
