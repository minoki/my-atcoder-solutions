-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr, intersperse)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import System.IO
import Data.Monoid

type Map = U.Vector Int

compose :: Int -> Map -> Map -> Map
compose !n xs ys = U.generate n (\i -> xs U.! (ys U.! i))

powMap :: Int -> Map -> Integer -> Map
powMap !n xs 0 = U.generate n id
powMap !n xs m = loop xs xs (m-1)
  where
    loop !acc !ys 0 = acc
    loop !acc !ys 1 = compose n acc ys
    loop !acc !ys m = case m `quotRem` 2 of
                        (m',0) -> loop acc (compose n ys ys) m'
                        (m',_) -> loop (compose n acc ys) (compose n ys ys) m'

main = do
  s <- BS.getLine
  let n = BS.length s
  let xs = U.generate n (\i -> if BS.index s i == 'R' then i + 1 else i - 1)
  let zs = powMap n xs (10^100)
  let result = U.create $ do
        vec <- UM.replicate n 0
        U.forM_ zs $ \i -> UM.modify vec (+1) i
        return vec
  BSB.hPutBuilder stdout $ (mconcat $ intersperse (BSB.char7 ' ') $ map BSB.intDec $ U.toList result) <> BSB.char7 '\n'
