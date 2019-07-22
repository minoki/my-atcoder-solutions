{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.ByteString.Char8 as BS

main = do
  [n,y] :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  let result = [ (man,gosen,sen)
               -- man >= 0, gosen >= 0, sen >= 0
               -- n = man + gosen + sen
               -- y == 10000*man + 5000*gosen + 1000*sen
               -- <=> y == 10000*man + 5000*gosen + 1000*(n - man - gosen)
               -- <=> y == 9000*man + 4000*gosen + 1000*n
               -- <=> (y - 9000*man - 1000*n) `quotRem` 4000 == (gosen, 0)
               | man <- [max 0 (((y + 5000 - 1) `quot` 5000) - n) .. ((y - 1000*n) `quot` 9000)]
               -- man >= 0: OK
               , (gosen, 0) <- return ((y - 9000*man - 1000*n) `quotRem` 4000)
               -- gosen >= 0
               -- <=> y - 9000*man - 1000*n >= 0
               -- <=> y - 1000*n >= 9000*man
               -- <=> (y - 1000*n) `quot` 9000 >= man
               , let sen = n - man - gosen
               -- sen >= 0
               -- <=> n - man - gosen >= 0
               -- <=> n - man - ((y - 9000*man - 1000*n) `quot` 4000) >= 0
               -- <=> 4000*n - 4000*man - y + 9000*man + 1000*n >= 0
               -- <=> 5000*n + 5000*man - y >= 0
               -- <=> man >= ((y + 5000 - 1) `quot` 5000) - n
               ]
      check (man,gosen,sen) = man >= 0 && gosen >= 0 && sen >= 0 && n == man + gosen + sen && y == 10000*man + 5000*gosen + 1000*sen
  -- if not (all check result)
  --  then putStrLn "error"
  --  else return ()
  putStrLn $ unwords $ map show $ case result of
    (man,gosen,sen):_ -> [man,gosen,sen]
    [] -> [-1,-1,-1]
