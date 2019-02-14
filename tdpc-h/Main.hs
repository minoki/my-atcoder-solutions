{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Control.Monad.ST
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Bits
import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import System.Environment
import Data.List
import Control.Monad.Reader

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

solve :: Int -> Int -> Word64 -> Int -> [(Int,Int,Int)] -> VM.STVector s (M.Map (Int,Word64) Int) -> ST s Int
solve !maxColors !maxWeight !colors !i [] memo = return 0
solve !maxColors 0 !colors !i _ memo = return 0
solve !maxColors !maxWeight !colors !i ((!w,!v,!c):xs) memo
  | popCount colors == maxColors && not (testBit colors c) = solve maxColors maxWeight colors (i + 1) xs memo
  | w > maxWeight = solve maxColors maxWeight colors (i + 1) xs memo
  | otherwise = do
      map <- VM.read memo i
      case M.lookup (maxWeight, colors) map of
        Just u -> return u
        Nothing -> do
          a <- solve maxColors maxWeight colors (i + 1) xs memo
          b <- solve maxColors (maxWeight - w) (setBit colors c) (i + 1) xs memo
          let !u = max a (b + v)
          VM.write memo i (M.insert (maxWeight, colors) u map)
          return u

-- returns [(value1,weight1,colors1),(value2,weight2,colors2),...] in the order value1 >= value2 >= value3 >= ...
solveL :: Int -> Int -> [(Int,Int,Int)] -> [(Int,Int,Word64)] -> [(Int,Int,Word64)]
solveL !maxColors !maxWeight [] ys = ys
solveL !maxColors 0 _ ys = ys
solveL !maxColors !maxWeight ((!w,!v,!c):xs) ys
  | w > maxWeight = solveL maxColors maxWeight xs ys
  | otherwise = solveL maxColors maxWeight xs
    $ merge ys [ (vv,ww,cc)
               | (v',w',colors) <- ys
               , let !ww = w + w'
               , ww <= maxWeight
               , let !cc = setBit colors c
               , popCount cc <= maxColors
               , let !vv = v' + v
               ]
  where
    merge [] ys = ys
    merge xs [] = xs
    merge xs@(x@(v,w,c):xss) ys@(y@(v',w',c'):yss)
      = case compare v v' of
          LT -> y : merge xs yss
          GT -> x : merge xss ys
          EQ | w <= w' && (c .&. c') == c -> x : merge xss ys -- x is better
             | w >= w' && (c .&. c') == c' -> y : merge xs yss -- y is better
             | otherwise -> x : y : merge xss yss

data Triple = Triple {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Word64
solveLU :: Int -> Int -> [(Int,Int,Int)] -> [Triple] -> [Triple]
solveLU !maxColors !maxWeight [] ys = ys
solveLU !maxColors 0 _ ys = ys
solveLU !maxColors !maxWeight ((!w,!v,!c):xs) ys
  | w > maxWeight = solveLU maxColors maxWeight xs ys
  | otherwise = solveLU maxColors maxWeight xs
                $ merge ys [ Triple vv ww cc
                           | Triple v' w' colors <- ys
                           , let !ww = w + w'
                           , ww <= maxWeight
                           , let !cc = setBit colors c
                           , popCount cc <= maxColors
                           , let !vv = v' + v
                           ]
  where
    merge [] ys = ys
    merge xs [] = xs
    merge xs@(x@(Triple v w c):xss) ys@(y@(Triple v' w' c'):yss)
      = case compare v v' of
          LT -> y : merge xs yss
          GT -> x : merge xss ys
          EQ | w <= w' && (c .&. c') == c -> x : merge xss ys -- x is better
             | w >= w' && (c .&. c') == c' -> y : merge xs yss -- y is better
             | otherwise -> x : y : merge xss yss

-- vec V.! v = (the minimal (weight,colors) needed to get the value v)
solveV :: Int -> Int -> [(Int,Int,Int)] -> V.Vector (S.Set (Int,Word64)) -> V.Vector (S.Set (Int,Word64))
solveV !maxWeight !maxColors [] vec = vec
solveV !maxWeight !maxColors ((!w,!v,!c):xs) vec
  -- | w > maxWeight = solveV maxWeight maxColors xs vec
  | otherwise = solveV maxWeight maxColors xs
    $ trim $ if v < V.length vec
             then V.generate (v + V.length vec)
                  $ \i -> if i < v
                          then vec V.! i
                          else let set = add $ vec V.! (i - v)
                               in if i < V.length vec
                                  then S.union (vec V.! i) set
                                  else set
             else V.generate (v + V.length vec)
                  $ \i -> if i < V.length vec
                          then vec V.! i
                          else if i < v
                               then S.empty
                               else add $ vec V.! (i - v)
  where trim v | S.null (V.last v) = trim (V.init v)
               | otherwise = v
        add set = S.filter (\(w',colors) -> w' <= maxWeight && popCount colors <= maxColors)
                  $ S.map (\(w',colors) -> (w' + w, setBit colors c)) set

main = do
  [n,w,c] <- parseInts <$> getLine
  -- 1 <= n <= 100, 1 <= w <= 10000, 1 <= c <= 50
  xs <- replicateM n $ do
    [wi,vi,ci] <- parseInts <$> getLine
    -- 1 <= wi, vi <= 10000
    -- 1 <= ci <= 50
    return (wi,vi,ci)
  let ss = sortOn (\(wi,vi,ci) -> (ci, fromIntegral vi / fromIntegral wi :: Double, vi, wi)) xs
  {-
  let sumW = sum [wi | (wi,_,_) <- xs]
  let allColors = foldl' setBit (0 :: Word64) [ci | (_,_,ci) <- xs]
  print (sumW, w)
  print (popCount allColors)
  -}
  let solveMemo = do let result = runST $ do
                           memo <- VM.replicate (n + 1) M.empty
                           solve c w 0 0 xs memo
                     print result
      solveList = do let (v,_,_):_ = solveL c w xs [(0,0,0)]
                     print v
      solveListU = do let Triple v _ _:_ = solveLU c w xs [Triple 0 0 0]
                      print v
      solveListUSorted = do let Triple v _ _:_ = solveLU c w ss [Triple 0 0 0]
                            print v
      solveVec = do let v = solveV w c xs (V.singleton (S.singleton (0,0)))
                    print (V.length v - 1)
      solveVecS = do let v = solveV w c ss (V.singleton (S.singleton (0,0)))
                     print (V.length v - 1)
  args <- getArgs
  case args of
    "memo":_ -> solveMemo
    "list":_ -> solveList
    "listU":_ -> solveListU
    "listUS":_ -> solveListUSorted
    "vector":_ -> solveVec
    "vectorS":_ -> solveVecS
    _ -> solveListUSorted
