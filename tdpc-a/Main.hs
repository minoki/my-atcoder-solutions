import Data.Char
import Data.List
import Data.Bits
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn :: IO Int
  ps <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ popCount (foldl' (\xs p -> (xs .|. (xs `shiftL` p))) 1 ps :: Integer)
