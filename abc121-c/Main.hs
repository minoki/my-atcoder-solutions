{-# LANGUAGE BangPatterns #-}
import Data.Int
import Data.List hiding (insert)
import qualified Data.ByteString.Char8 as BS

data Tree = Empty
          | Bin {- key (yen) -} {-# UNPACK #-} !Int64
                {- value (number of drink) -} {-# UNPACK #-} !Int
                {- total cost (yen) -} {-# UNPACK #-} !Int64
                {- total number of drink -} {-# UNPACK #-} !Int
                Tree Tree

insert :: Int64 -> Int -> Tree -> Tree
insert !k !v Empty = Bin k v (k * fromIntegral v) v Empty Empty
insert !k !v (Bin k' v' tc tn s t) = case compare k k' of
  LT -> Bin k' v' tc' tn' (insert k v s) t
  EQ -> Bin k (v + v') tc' tn' s t
  GT -> Bin k' v' tc' tn' s (insert k v t)
  where tc' = tc + k * fromIntegral v
        tn' = tn + v

toList :: Tree -> [(Int64,Int)]
toList = go []
  where
    go xs Empty = xs
    go xs (Bin k v _ _ s t) = go ((k,v) : go xs t) s

total :: Tree -> (Int64,Int)
total Empty = (0,0)
total (Bin _ _ tc tn _ _) = (tc,tn)

main = do
  [n,m] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  ab <- fmap (foldl' (flip $ uncurry insert) Empty) $ sequence $ replicate n $ do
    [a,b] <- map (read . BS.unpack) . BS.words <$> BS.getLine
    return (a :: Int64, fromIntegral b :: Int) -- (a :: Int64, b :: Int)
  let solve !k !yen (Bin a b _ _ s t) | k < ln = solve k yen s
                                      | k <= ln + b = yen + lc + a * fromIntegral (k - ln)
                                      | otherwise = solve (k - ln - b) (yen + lc + a * fromIntegral b) t
        where (lc,ln) = total s
  print $ solve m 0 ab

naiveMain = do
  [n,m] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  ab <- fmap sort $ sequence $ replicate n $ do
    [a,b] <- map (read . BS.unpack) . BS.words <$> BS.getLine
    return (a :: Int64, b :: Int64)
  let solve !k !yen ((a,b):xs) | k <= b = yen + a * k
                               | otherwise = solve (k - b) (yen + a * b) xs
  print $ solve (fromIntegral m) 0 ab
