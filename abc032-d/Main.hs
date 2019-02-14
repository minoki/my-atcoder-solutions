{-# LANGUAGE BangPatterns #-}
import Control.Monad
import qualified Data.IntMap as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Mutable as VM
import Data.Array.IO
import Data.Int
import System.Environment

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

-- wi's are small
-- vec V.! w = (the maximum value obtained with the weight w)
solveV :: Int -> [(Int,Int)] -> V.Vector Int -> V.Vector Int
solveV !maxW [] map = map
solveV !maxW ((!v,!w):xs) map = solveV maxW xs
  $ if w < V.length map
    then V.generate (min (w + V.length map) (maxW + 1))
         $ \i -> if i < w
                 then map V.! i
                 else if i < V.length map
                      then max (map V.! i) ((map V.! (i - w)) + v)
                      else (map V.! (i - w)) + v
    else let l = (min (w + V.length map) (maxW + 1))
         in if w < l
            then V.generate l
                 $ \i -> if i < V.length map
                         then map V.! i
                         else if i < w
                              then 0
                              else (map V.! (i - w)) + v
            else map

-- vi's are small
-- map M.! v = (the minimum weight needed to get the value v)
solveM :: Int -> [(Int,Int)] -> M.IntMap Int -> M.IntMap Int
solveM !maxW [] map = map
solveM !maxW ((!v,!w):xs) map
  | w <= maxW = solveM maxW xs
    $ M.unionWith min map
    $ M.fromAscList [ (vv, ww)
                    | (v',w') <- M.toAscList map
                    , let !ww = w + w'
                    , ww <= maxW
                    , let !vv = v' + v
                    ]
  | otherwise = solveM maxW xs map

solveL :: Int -> [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
solveL !maxW [] map = map
solveL !maxW ((!v,!w):xs) map
  | w <= maxW = solveL maxW xs
    $ merge map [ (vv, ww)
                | (v',w') <- map
                , let !ww = w + w'
                , ww <= maxW
                , let !vv = v' + v
                ]
  | otherwise = solveL maxW xs map
  where
    merge [] ys = ys
    merge xs [] = xs
    merge xs@(x@(v,w):xss) ys@(y@(v',w'):yss)
      = case compare v v' of
          LT -> y : merge xs yss
          GT -> x : merge xss ys
          EQ -> (v, min w w') : merge xss yss

solveA :: Int -> Int -> [(Int,Int)] -> IOUArray (Int,Int) Int -> IO Int
solveA !0 !_ _ arr = return 0
solveA !maxW !i [] arr = return 0
solveA !maxW !i ((!v,!w):xs) arr = do
  u <- readArray arr (i, maxW)
  if u == -1
    then do u' <- do a <- solveA maxW (i+1) xs arr
                     if w > maxW
                       then return a
                       else do b <- solveA (maxW - w) (i+1) xs arr
                               return $ max a (b+v)
            writeArray arr (i, maxW) u'
            return u'
    else return u

solveAM :: Int -> Int -> Int -> [(Int,Int)] -> VM.IOVector (M.IntMap Int) -> IO Int
solveAM !0 !sumW !_ _ arr = return 0
solveAM !maxW !sumW !i [] arr = return 0
solveAM !maxW !sumW !i ((!v,!w):xs) arr = do
  map <- VM.read arr i
  case M.lookup maxW map of
    Just u -> return u
    Nothing -> do
      u <- if sumW <= maxW
           then return $ v + sum [vi | (vi,_) <- xs]
           else do a <- solveAM maxW (sumW - w) (i+1) xs arr
                   if w > maxW
                     then return a
                     else do b <- solveAM (maxW - w) (sumW - w) (i+1) xs arr
                             return $ max a (b+v)
      VM.write arr i (M.insert maxW u map)
      return u

-- vec V.! v = (the minimum weight needed to get the value v)
solveVV :: Int -> [(Int,Int)] -> V.Vector Int -> V.Vector Int
solveVV !maxW [] map = map
solveVV !maxW ((!v,!w):xs) map
  | w <= maxW = solveVV maxW xs
    $ trim $ if v < V.length map
             then V.generate (v + V.length map)
                  $ \i -> if i < v
                          then map V.! i
                          else if i < V.length map
                               then let ww = (map V.! (i - v)) + w
                                    in if ww <= maxW
                                       then min (map V.! i) ww
                                       else map V.! i
                               else (map V.! (i - v)) + w
             else let l = v + V.length map
                  in V.generate l
                     $ \i -> if i < V.length map
                             then map V.! i
                             else if i < v
                                  then maxW + 1 -- infinity
                                  else (map V.! (i - v)) + w
  | otherwise = solveVV maxW xs map
  where trim v | V.last v > maxW = trim (V.init v)
               | otherwise = v

main = do
  [n,maxW] <- parseInts <$> getLine
  -- 1 <= n <= 200, 1 <= maxW <= 10^9
  xs <- replicateM n $ do
    [vi,wi] <- parseInts <$> getLine
    -- 1 <= vi <= 10^9
    -- 1 <= wi <= 10^9
    return (vi,wi)
  let sumW = sum [wi | (_,wi) <- xs]
  let solve1 = do
        -- dataset 2
        -- wi's are small
        -- uses vector
        let m = solveV maxW xs (V.singleton 0)
        print (V.maximum m)
      solve2 = do
        -- vi's are small
        -- uses map
        let m = solveM maxW xs (M.singleton 0 0)
        print (fst $ M.findMax m)
      solve2L = do
        -- vi's are small
        -- uses list
        let m = solveL maxW xs [(0,0)]
        print (fst $ head m)
      solve3 = do
        -- memoize with vector + map
        arr <- VM.replicate (n + 1) M.empty
        ans <- solveAM maxW sumW 0 xs arr
        print ans
      solve4 = do
        -- memoize with array
        arr <- newArray ((0, 0), (n, maxW)) (-1)
        ans <- solveA maxW 0 xs arr
        print ans
      solve5 = do -- dataset 3
        -- vi's are small
        -- uses vector
        let m = solveVV maxW xs (V.singleton 0)
        print (V.length m - 1)
  args <- getArgs
  case args of
    "vector":_ -> solve1
    "vectorV":_ -> solve5
    "map":_ -> solve2
    "list":_ -> solve2L
    "memo":_ -> solve3
    "memoA":_ -> solve4
    _ -> if sumW <= maxW
         then print $ sum [vi | (vi,_) <- xs]
         else if maxW <= 200000 -- all (\(_,wi) -> wi <= 1000) xs
              then solve1
              else solve2L
