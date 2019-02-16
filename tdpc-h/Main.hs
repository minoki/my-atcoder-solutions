{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Control.Monad.ST
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import qualified Data.Set as S
import Data.Bits
import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Array.ST
import System.Environment
import Data.List
import Control.Monad.Reader

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

-- memo : i -> (maxWeight, colors) -> value
solveMemo :: Int -> Int -> Word64 -> Int -> [(Int,Int,Int)] -> ReaderT (VM.STVector s (M.Map (Int,Word64) Int)) (ST s) Int
solveMemo !maxColors !maxWeight !colors !i [] = return 0
solveMemo !maxColors 0 !colors !i _ = return 0
solveMemo !maxColors !maxWeight !colors !i ((!w,!v,!c):xs)
  | popCount colors == maxColors && not (testBit colors c) = solveMemo maxColors maxWeight colors (i + 1) xs
  | w > maxWeight = solveMemo maxColors maxWeight colors (i + 1) xs
  | otherwise = do
      memo <- ask
      map <- lift $ VM.read memo i
      case M.lookup (maxWeight, colors) map of
        Just u -> return u
        Nothing -> do
          a <- solveMemo maxColors maxWeight colors (i + 1) xs
          b <- solveMemo maxColors (maxWeight - w) (setBit colors c) (i + 1) xs
          let !u = max a (b + v)
          memo <- ask
          lift $ VM.write memo i (M.insert (maxWeight, colors) u map)
          return u

-- memo : (i, maxWeight) -> colors -> value
solveMemo2 :: Int -> Int -> Word64 -> Int -> [(Int,Int,Int)] -> ReaderT (STArray s (Int,Int) (M.Map Word64 Int)) (ST s) Int
solveMemo2 !maxColors !maxWeight !colors !i [] = return 0
solveMemo2 !maxColors 0 !colors !i _ = return 0
solveMemo2 !maxColors !maxWeight !colors !i ((!w,!v,!c):xs)
  | popCount colors == maxColors && not (testBit colors c) = solveMemo2 maxColors maxWeight colors (i + 1) xs
  | w > maxWeight = solveMemo2 maxColors maxWeight colors (i + 1) xs
  | otherwise = do
      memo <- ask
      map <- lift $ readArray memo (i, maxWeight)
      case M.lookup colors map of
        Just u -> return u
        Nothing -> do
          a <- solveMemo2 maxColors maxWeight colors (i + 1) xs
          b <- solveMemo2 maxColors (maxWeight - w) (setBit colors c) (i + 1) xs
          let !u = max a (b + v)
          memo <- ask
          lift $ writeArray memo (i, maxWeight) (M.insert colors u map)
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

-- returns [(value1,set of (weight1,colors1)),(value2,set of (weight2,colors2)),...] in the order value1 > value2 > value3 > ...
solveListSet :: Int -> Int -> [(Int,Int,Int)] -> [(Int,S.Set (Int,Word64))] -> [(Int,S.Set (Int,Word64))]
solveListSet !maxColors !maxWeight [] ys = ys
solveListSet !maxColors 0 _ ys = ys
solveListSet !maxColors !maxWeight ((!w,!v,!c):xs) ys
  | w > maxWeight = solveListSet maxColors maxWeight xs ys
  | otherwise = solveListSet maxColors maxWeight xs
    $ merge ys [ (vv,set' {- ww,cc -})
               | (v',set {- w',colors -}) <- ys
               , let set' = S.fromAscList
                            [ (ww, cc)
                            | (w',colors) <- S.toAscList set
                            , let !ww = w + w'
                            , ww <= maxWeight
                            , let !cc = setBit colors c
                            , popCount cc <= maxColors
                            ]
               , not (S.null set')
               , let !vv = v' + v
               ]
  where
    merge [] ys = ys
    merge xs [] = xs
    merge xs@(x@(v,set):xss) ys@(y@(v',set'):yss)
      = case compare v v' of
          LT -> y : merge xs yss
          GT -> x : merge xss ys
          EQ -> (v,set'') : merge xss yss
            where set'' = S.union
                    (S.fromAscList [ s | s@(w,c) <- S.toAscList set
                                       , all (\(w',c') -> w <= w' || (c .&. c') /= c) (S.toList set')
                                       ])
                    (S.fromAscList [ s | s@(w,c) <- S.toAscList set'
                                       , all (\(w',c') -> w <= w' || (c .&. c') /= c) (S.toList set)
                                       ])
                    {-
      | w <= w' && (c .&. c') == c -> x : merge xss ys -- x is better
             | w >= w' && (c .&. c') == c' -> y : merge xs yss -- y is better
             | otherwise -> x : y : merge xss yss
-}

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

solveByColor :: Int -> Int -> [(Int,[(Int,Int)])] -> ML.Map Int (VU.Vector Int)
solveByColor maxWeight maxColor [] = let v = VU.replicate (maxWeight + 1) 0
                                     in ML.fromList [(k, v) | k <- [0..maxColor]]
solveByColor maxWeight maxColor ((_c,ys):xs)
  = let map = solveByColor maxWeight maxColor xs
    in ML.fromList [ if k == 0 then (0, map ML.! 0) else (k, v0)
                   | k <- [0..maxColor]
                   , let (v0, v1) = VU.unzip (loop ys (VU.zip (map ML.! k) (map ML.! (k-1)))) :: (VU.Vector Int, VU.Vector Int)
                   ]
  where
    loop [] vv = vv
    loop ((!w,!v):ys) vv = loop ys $ VU.create $ do
      ww <- VUM.replicate (maxWeight + 1) (0, 0)
      forM_ [0..maxWeight] $ \i ->
        if i < w
          then VUM.write ww i (vv VU.! i)
          else do let (a0, a1) = vv VU.! i
                      (_, a1') = vv VU.! (i - w)
                      b0 = max a0 (a1' + v)
                      b1 = max a1 (a1' + v)
                  VUM.write ww i (b0,b1)
      return ww

main = do
  [n,w,c] <- parseInts <$> getLine
  -- 1 <= n <= 100, 1 <= w <= 10000, 1 <= c <= 50
  xs <- replicateM n $ do
    [wi,vi,ci] <- parseInts <$> getLine
    -- 1 <= wi, vi <= 10000
    -- 1 <= ci <= 50
    return (wi,vi,ci)
  let ss = sortOn (\(wi,vi,ci) -> (fromIntegral vi / fromIntegral wi :: Double, vi, ci, wi)) xs
  {-
  let sumW = sum [wi | (wi,_,_) <- xs]
  let allColors = foldl' setBit (0 :: Word64) [ci | (_,_,ci) <- xs]
  print (sumW, w)
  print (popCount allColors)
  -}
  let runSolveMemo = do
        let result = runST $ do
              memo <- VM.replicate (n + 1) M.empty
              runReaderT (solveMemo c w 0 0 xs) memo
        print result
      runSolveMemo2 = do
        let result = runST $ do
              memo <- newArray ((0, 0), (n, w)) M.empty
              runReaderT (solveMemo2 c w 0 0 xs) memo
        print result
      solveList = do let (v,_,_):_ = solveL c w xs [(0,0,0)]
                     print v
      solveListU = do let Triple v _ _:_ = solveLU c w xs [Triple 0 0 0]
                      print v
      solveListUSorted = do let Triple v _ _:_ = solveLU c w ss [Triple 0 0 0]
                            print v
      runSolveListSet = do let (v,_):_ = solveListSet c w ss [(0, S.singleton (0,0))]
                           print v
      solveVec = do let v = solveV w c xs (V.singleton (S.singleton (0,0)))
                    print (V.length v - 1)
      solveVecS = do let v = solveV w c ss (V.singleton (S.singleton (0,0)))
                     print (V.length v - 1)
      solveColor = do let s = M.toList (M.fromListWith (++) [ (ci,[(wi,vi)]) | (wi,vi,ci) <- xs ])
                          resultMap = solveByColor w c s
                      print ((resultMap ML.! c) VU.! w)
  args <- getArgs
  case args of
    "memo":_ -> runSolveMemo
    "memo2":_ -> runSolveMemo2
    "list":_ -> solveList
    "listU":_ -> solveListU
    "listUS":_ -> solveListUSorted
    "listSet":_ -> runSolveListSet
    "vector":_ -> solveVec
    "vectorS":_ -> solveVecS
    "color":_ -> solveColor
    _ -> solveColor
