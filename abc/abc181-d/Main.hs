-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
import Data.List
import qualified Data.ByteString.Char8 as BS

shortCase :: String -> Bool
shortCase xs = or $ do
  ys <- permutations xs
  return (read @Int ys `rem` 8 == 0)

multiplesOf8 :: [[(Char, Int)]]
multiplesOf8 = [ if c0 == c2 then
                   [(c0,3)]
                 else if c0 == c1 then
                   [(c0,2),(c2,1)]
                 else if c1 == c2 then
                   [(c0,1),(c1,2)]
                 else
                   [(c0,1),(c1,1),(c2,1)]
               | n <- [13 .. 124]
               , let [c0,c1,c2] = sort $ show (8 * n)
               ]

longCase :: BS.ByteString -> Bool
longCase s = or $ do
  m <- multiplesOf8
  return $ all (\(c,n) -> BS.count c s >= n) m

main = do
  s <- BS.getLine
  let result = if BS.length s <= 2 then
                 shortCase (BS.unpack s)
               else
                 longCase s
  putStrLn $ if result then "Yes" else "No"
