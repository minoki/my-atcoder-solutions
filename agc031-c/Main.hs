import Data.Bits
import qualified Data.Vector.Unboxed as V
import Data.List
import Data.Monoid
-- import GHC.Stack (HasCallStack)

-- path1 n a b xs
--   n == length xs + 1
--   popCount (a `xor` b) == 1
path1 :: Int -> Int -> Int -> [Int] -> V.Vector Int
-- path1 n a b xs | not (n == length xs + 1) = error "path1: precondition 1"
-- path1 n a b xs | not (popCount (a `xor` b) == 1) = error "path1: precondition 2"
path1 1 a b [] = V.fromList [a,b] -- [0,2^k] popCount (a `xor` b) == 1
path1 n a b (k:ks) = let v = path1 (n - 1) a b ks
                         (v0,v1) = V.splitAt (2^(n-2)) v
                         y = 2^k
                     in v0 <> V.map (`xor` y) (V.reverse v0) <> V.map (`xor` y) (V.reverse v1) <> v1

-- pathN m a b xs availBits
pathN :: Int -> Int -> Int -> [Int] -> [Int] -> V.Vector Int
pathN m a b [i] availBits = path1 m a b availBits
pathN m a b (i0:i1:is) availBits = let a' = a `xor` bit i0
                                       b' = a' `xor` bit i1
                                   in path1 (m-1) a a' (is ++ availBits) <> pathN (m-1) b' b is (i0 : availBits)

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

{-
myAssert :: HasCallStack => Bool -> IO ()
myAssert True = return ()
myAssert False = error "Assertion failed"
-}

main = do
  [n,a,b] <- parseInts <$> getLine
  let c = a `xor` b
  if odd (popCount c)
    then do putStrLn "YES"
            let bitsDiffer = [i | i <- [0..n-1], testBit c i]
                bitsSame = [i | i <- [0..n-1], not (testBit c i)]
            let xs = pathN n a b bitsDiffer bitsSame
                xss = V.toList xs
            putStrLn $ intercalate " " $ map show xss
            {-
            let differByOne a b = popCount (a `xor` b) == 1
            myAssert (and $ zipWith differByOne xss (tail xss))
            myAssert (a == V.head xs && b == V.last xs)
            myAssert (sort xss == [0..2^n-1])
            -}
    else putStrLn "NO"
