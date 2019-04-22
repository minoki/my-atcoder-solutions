import qualified Data.ByteString.Char8 as BS
main = do
  [Just (a,_), Just (b,_)] <- map BS.readInt . BS.words <$> BS.getLine
  if even (a * b)
    then putStrLn "Even"
    else putStrLn "Odd"
