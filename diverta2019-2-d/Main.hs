-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr, sort)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Ord
import Data.Coerce

sortDown :: forall a. Ord a => [a] -> [a]
sortDown = coerce (sort :: [Down a] -> [Down a])

exchangeOne :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
exchangeOne !gA !sA !bA !gB !sB !bB !n =
  let dg = gB - gA
      ds = sB - sA
      db = bB - bA
      l = sortDown $ filter (\(d,x) -> d > 0) [(dg, gA), (ds, sA), (db, bA)]
  in case l of
       [] -> n
       [(dx,xA)] -> let (qx,rx) = n `quotRem` xA
                        -- A で qx * gA 個のどんぐりを売却し、金 qx グラムを得る。
                        -- B で金 qx グラムを売却し、 qx * gB 個のどんぐりを得る。
                    in n + qx * dx
       [(dx,xA),(dy,yA)] -> maximum [ n + x * dx + y * dy
                                    | x <- [0..n `quot` xA]
                                    , let y = (n - x * xA) `quot` yA
                                    ]
       [(dx,xA),(dy,yA),(dz,zA)] -> maximum [ n + x * dx + y * dy + z * dz
                                            | x <- [0..n `quot` xA]
                                            , y <- [0..(n - x * xA) `quot` yA]
                                            , let z = (n - x * xA - y * yA) `quot` zA
                                            ]

main = do
  n <- readLn
  [gA,sA,bA] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  [gB,sB,bB] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ exchangeOne gB sB bB gA sA bA $ exchangeOne gA sA bA gB sB bB n
