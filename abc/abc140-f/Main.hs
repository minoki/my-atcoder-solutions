-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IntMap
import Data.List

takeLT :: Int -> IntMap.IntMap Int -> Maybe (Int, IntMap.IntMap Int)
takeLT k m = case IntMap.lookupLT k m of
               Just (l, a) | a == 1 -> Just (l, IntMap.delete l m)
                           | otherwise -> Just (l, IntMap.update (const $! Just $! a-1) l m)
               Nothing -> Nothing

check :: IntMap.IntMap Int -> IntMap.IntMap Int -> Bool
check m m' | IntMap.null m' = True
           | otherwise = case IntMap.foldrWithKey (\ !k !a acc -> case acc of Just (_,_) -> iterate (f k) acc !! a; Nothing -> Nothing) (Just (m, m')) m of
                           Just (m2, m'2) -> check m2 m'2
                           Nothing -> False
  where f :: Int -> Maybe (IntMap.IntMap Int, IntMap.IntMap Int) -> Maybe (IntMap.IntMap Int, IntMap.IntMap Int)
        f !k (Just (!m, !m')) = case takeLT k m' of
                                  Just (l, !m'2) -> let !m2 = IntMap.insertWith (+) l 1 m
                                                    in Just (m2, m'2)
                                  Nothing -> Nothing
        f k Nothing = Nothing

main = do
  n <- readLn :: IO Int -- 1 <= n <= 18
  xs <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let m = foldl' (\m x -> IntMap.insertWith (+) x 1 m) IntMap.empty xs
  let result = case IntMap.deleteFindMax m of
                 ((x, a), m') | a == 1 -> check (IntMap.singleton x 1) m'
                              | otherwise -> False
  if result
    then putStrLn "Yes"
    else putStrLn "No"
