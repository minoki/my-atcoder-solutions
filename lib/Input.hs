{-# LANGUAGE ScopedTypeVariables #-}
module Input where
import Data.List (unfoldr)
import Data.Char (isSpace)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  _ :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM m $ do
    [x,y,z] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (x-1,y-1,z)
  return ()
