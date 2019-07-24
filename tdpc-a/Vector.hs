import Data.Char
import Data.List
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as U

step :: Int -> U.Vector Bool -> U.Vector Bool
step p xs = U.accumulate (||) (xs <> U.replicate p False)
            $ U.zip (U.enumFromN p $ U.length xs) xs

main = do
  n <- readLn :: IO Int
  ps <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ U.length $ U.filter id $ foldr step (U.singleton True) ps
