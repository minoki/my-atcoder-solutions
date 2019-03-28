{-# LANGUAGE BangPatterns #-}
import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  let loop 0 !a !b !c = return (a,b,c)
      loop i !a !b !c = do
        [a',b',c'] <- map (read . BS.unpack) . BS.words <$> BS.getLine
        -- a,b,c <= 10^4
        loop (i - 1) (max b c + a') (max a c + b') (max a b + c')
  (a,b,c) <- loop n 0 0 0
  {-
  happiness <- V.replicateM n $ do
    [a,b,c] <- map (read . BS.unpack) . BS.words <$> BS.getLine
    -- a,b,c <= 10^4
    return (a,b,c :: Int)
  let (a,b,c) = V.foldl1' (\(!a,!b,!c) (!a',!b',!c') -> (max b c + a', max a c + b', max a b + c')) happiness
  -}
  print (max a (max b c))
