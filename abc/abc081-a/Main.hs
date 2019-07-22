import qualified Data.ByteString.Char8 as BS
main = do
  s <- BS.getLine
  print $ BS.count '1' s
