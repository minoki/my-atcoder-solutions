import qualified Data.ByteString.Char8 as BS
main = do
  s <- BS.getLine
  -- 1 <= BS.length s <= 10^5
  let zeros = BS.count '0' s
      ones = BS.count '1' s
  print $ 2 * min zeros ones
