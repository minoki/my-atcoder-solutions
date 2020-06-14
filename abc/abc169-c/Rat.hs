{-# LANGUAGE TypeApplications #-}
import Numeric (readFloat)

main = do
  [s,t] <- words <$> getLine
  let a = read @Integer s
      [(b,"")] = readFloat @Rational t
  print (truncate $ fromInteger a * b :: Integer)
