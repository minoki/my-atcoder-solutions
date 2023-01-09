import Data.Int (Int64)
import qualified Test.QuickCheck as QC
import Math.NumberTheory.Primes

test :: Int64 -> Bool
test n = truncate (sqrt (fromIntegral (n^2 :: Int64) :: Double)) < n

examples :: [Int64]
examples = [a | p <- [nextPrime 94906266..precPrime 3037000499], let a = fromInteger (unPrime p), test a]

prop :: QC.Property
prop = let gen = QC.choose (94906266, 3037000499) -- 94906265^2 < 2^53 < 94906266^2, 3037000499^2 < 2^63 - 1 < 3037000500^2
       in QC.forAll gen (\n -> truncate (fromIntegral (n^2 :: Int64) :: Double) < n^2)

-- main = QC.quickCheck prop

main = print $ take 10 [n | n <- [94906266..3037000499], test n]

-- main = print examples
