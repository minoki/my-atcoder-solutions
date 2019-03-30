import qualified Data.ByteString.Char8 as BS

main = do
  [a,b,c] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  let _ = a :: Int
  if a == b && b == c
    then putStrLn "Yes"
    else putStrLn "No"
