import Test.DocTest

main :: IO ()
main = doctest
       [ "-ilib"
       , "lib/Primes.hs"
       ]
