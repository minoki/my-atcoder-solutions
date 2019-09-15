-- https://github.com/minoki/my-atcoder-solutions
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr, foldl')
import Control.Monad (mapM)
import qualified Data.ByteString.Char8 as BS
import Data.Bits (xor)
import Data.Maybe (catMaybes)

main = do
  n <- readLn :: IO Int
  xs <- map fromIntegral . unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- assume Int is 64-bit
  let s :: Int64
      s = foldl' xor 0 xs
  print $ maximum $ do t <- foldl' xor 0 . catMaybes <$> mapM (\x -> [Nothing, Just x]) xs
                       return $ t + (s `xor` t)
