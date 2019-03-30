import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed as V

main = do
  [n,m,c] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  b <- V.fromListN m . map (read . BS.unpack) . BS.words <$> BS.getLine
  result <- V.replicateM n $ do
    a <- V.fromListN m . map (read . BS.unpack) . BS.words <$> BS.getLine
    return $ if (V.sum $ V.zipWith (*) a b) + c > (0 :: Int) then 1 else 0
  print $ (V.sum result :: Int)
