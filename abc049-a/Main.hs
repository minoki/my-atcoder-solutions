-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BS

stripR :: BS.ByteString -> BS.ByteString
stripR s = case BS.uncons s of
             Just ('r', s') -> s'
             _ -> s

solve :: BS.ByteString -> Bool
solve s | BS.null s = True
        | "dreamerase" `BS.isPrefixOf` s = solve $ stripR $ BS.drop 10 s
        | "dreamer" `BS.isPrefixOf` s = solve $ BS.drop 7 s
        | "dream" `BS.isPrefixOf` s = solve $ BS.drop 5 s
        | "erase" `BS.isPrefixOf` s = solve $ stripR $ BS.drop 5 s
        | otherwise = False
-- stripPrefix :: ByteString -> ByteString -> Maybe ByteString is not available

main = do
  s <- BS.getLine
  putStrLn $ if solve s then "YES" else "NO"
