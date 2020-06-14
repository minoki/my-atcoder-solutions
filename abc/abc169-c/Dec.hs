{-# LANGUAGE TypeApplications #-}
import Data.Fixed

main = do
  [s,t] <- words <$> getLine
  let a = read @Integer s
      b = read @Centi t
  print (truncate $ fromInteger a * b :: Integer)
