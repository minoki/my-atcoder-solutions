{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Char
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS

readIntTriplet :: BS.ByteString -> (Int, Int, Int)
readIntTriplet s = let Just (a, s') = BS.readInt s
                       Just (b, s'') = BS.readInt $ BS.dropWhile isSpace s'
                       Just (c, _) = BS.readInt $ BS.dropWhile isSpace s''
                   in (a, b, c)

main = do
  n <- readLn
  let loop 0 !a !b !c = return (a,b,c)
      loop i !a !b !c = do
        (a',b',c') <- readIntTriplet <$> BS.getLine
        -- a,b,c <= 10^4
        loop (i - 1) (max b c + a') (max a c + b') (max a b + c')
  (a,b,c) <- loop n 0 0 0
  print (max a (max b c))

main2 = do
  n <- readLn
  happiness <- V.replicateM n (readIntTriplet <$> BS.getLine)
  let (a,b,c) = V.foldl1' (\(!a,!b,!c) (!a',!b',!c') -> (max b c + a', max a c + b', max a b + c')) happiness
  print (max a (max b c))
