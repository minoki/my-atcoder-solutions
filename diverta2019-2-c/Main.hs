-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr, sort)
import Control.Monad (forM_)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  -- n >= 2
  xs <- U.fromListN n . sort . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let (xs0, xs1) = U.span (< 0) xs
  let (r, zs)
        | U.length xs0 == 0 =
            let xs10 = U.head xs1
                xs11 = U.tail xs1
                loop 0 !y = (U.head xs11, y) : []
                loop i !y = let !z = xs11 U.! i
                            in (y, z) : loop (i - 1) (y - z)
            in (U.sum xs11 - xs10, loop (U.length xs11 - 1) xs10)
        | U.length xs1 == 0 =
            let xs00 = U.init xs0
                xs01 = U.last xs0
                loop i !y | i < 0 = []
                          | otherwise = let !z = xs00 U.! i
                                        in (y, z) : loop (i - 1) (y - z)
            in (xs01 - U.sum xs00, loop (U.length xs00 - 1) xs01)
        | otherwise =
            let xs00 = U.init xs0
                xs01 = U.last xs0
                xs10 = U.head xs1
                xs11 = U.tail xs1
                loopP i !y | i < 0 = (xs10, y) : loopN (U.length xs00 - 1) (xs10 - y)
                           | otherwise = let !z = xs11 U.! i
                                         in (y, z) : loopP (i - 1) (y - z)
                loopN i !y | i < 0 = []
                           | otherwise = let !z = xs00 U.! i
                                         in (y, z) : loopN (i - 1) (y - z)
            in (U.sum xs1 - U.sum xs0, loopP (U.length xs11 - 1) xs01)
  print r
  forM_ zs $ \(x,y) ->
    putStrLn $ concat [show x, " ", show y]
