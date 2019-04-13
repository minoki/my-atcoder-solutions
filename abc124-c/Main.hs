import qualified Data.ByteString.Char8 as BS

main = do
  s <- BS.getLine
  let bw = length $ filter (\(x,y) -> x /= y) $ zip (BS.unpack s) (cycle "01")
      wb = BS.length s - bw
  print (min bw wb)
