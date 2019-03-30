import qualified Data.ByteString.Char8 as BS

main = do
  n <- readLn :: IO Int
  s <- BS.getLine
  let a = BS.foldl' (\n c -> if c == 'R' then n + 1 else n) 0 s
      b = n - a
  if a > b
    then putStrLn "Yes"
    else putStrLn "No"
