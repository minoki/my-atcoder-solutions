-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap
import Data.Foldable

nextOccurrenceTable :: U.Vector Int -> (U.Vector Int, IntMap.IntMap Int)
nextOccurrenceTable v = runST $ do
  let n = U.length v
  w <- UM.new n
  let go p !i = do let a = v U.! i
                   case IntMap.lookup a p of
                     Nothing -> UM.write w i (-1)
                     Just j -> UM.write w i j
                   return $! IntMap.insert a i p
  p <- foldlM go IntMap.empty [n-1,n-2..0]
  result <- U.unsafeFreeze w
  return (result, p)

stepI :: U.Vector Int -> (U.Vector Int, IntMap.IntMap Int) -> Int -> Int
stepI v (nt,m) a | a == 0 = go 0
                 | otherwise = go (m IntMap.! a + 1)
  where
    go !i | i >= U.length v = 0
          | otherwise = let j = nt U.! i
                        in if j == -1
                           then v U.! i
                           else go (j + 1)

period :: U.Vector Int -> (U.Vector Int, IntMap.IntMap Int) -> Int
period v nt = 1 + (length $ takeWhile (/= 0) $ tail $ iterate (stepI v nt) 0)

lastStep :: U.Vector Int -> (U.Vector Int, IntMap.IntMap Int) -> Int -> U.Vector Int
lastStep v (nt,m) a = U.create $ do
  mv <- UM.new (U.length v)
  let go !i !l | i >= U.length v = return (UM.take l mv)
               | otherwise = let j = nt U.! i
                             in if j == -1
                                then do UM.write mv l (v U.! i)
                                        go (i + 1) (l + 1)
                                else go (j + 1) l
  go (if a == 0 then 0 else m IntMap.! a + 1) 0

solve :: Int -> Int64 -> U.Vector Int -> U.Vector Int
solve !n !k xs = let nt = nextOccurrenceTable xs
                     p = period xs nt
                     r = fromIntegral (k `rem` fromIntegral p)
                 in if r == 0
                    then U.empty
                    else let s = iterate (stepI xs nt) 0 !! (r - 1)
                         in lastStep xs nt s

main = do
  [n',k'] <- unfoldr (BS.readInteger . BS.dropWhile isSpace) <$> BS.getLine
  let n = fromIntegral n' :: Int
      k = fromIntegral k' :: Int64
  xs <- U.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  -- let result = naiveSolution n k xs
  let result = solve n k xs
  putStrLn $ unwords $ map show (U.toList result)

--

naiveSolution :: Int -> Int64 -> U.Vector Int -> U.Vector Int
naiveSolution !n !k xs = U.create $ do
  mv <- UM.new (2 * n)
  n <- foldlM (\n _ -> naiveStepV mv n xs) 0 [1..k]
  return $ UM.take n mv

naiveStep :: UM.MVector s Int -> Int -> Int -> ST s Int
naiveStep mv !n !x = do
  let loop !i | i >= n = return Nothing
              | otherwise = do
                  y <- UM.read mv i
                  if x == y
                    then return (Just i)
                    else loop (i+1)
  found <- loop 0
  case found of
    Nothing -> UM.write mv n x >> return (n+1)
    Just i -> return i

naiveStepV :: UM.MVector s Int -> Int -> U.Vector Int -> ST s Int
naiveStepV mv !n v = foldlM_UV (\n' x -> naiveStep mv n' x) n v

foldlM_UV :: (U.Unbox a, Monad m) => (b -> a -> m b) -> b -> U.Vector a -> m b
foldlM_UV f a = U.foldl (\m x -> m >>= \b -> f b x) (pure a)
