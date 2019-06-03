-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as BS

main = do
  [n,a,b,c,d] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  s <- BS.getLine
  let ss = BS.drop a (BS.take (max c d) s)
  let answer | "##" `BS.isInfixOf` ss = False
             | d > c = True
             | otherwise = "..." `BS.isInfixOf` BS.drop (b-2) (BS.take (min c d + 1) s)
  putStrLn $ if answer then "Yes" else "No"
