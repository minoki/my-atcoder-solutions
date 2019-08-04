-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let result = U.foldr' (\x h -> case h of
                            Just h' | x <= h'+1 -> Just $! min h' x
                            _ -> Nothing
                        ) (Just $ 10^9) xs
  case result of
    Just _ -> putStrLn "Yes"
    Nothing -> putStrLn "No"
