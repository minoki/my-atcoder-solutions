-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as BS

bounds :: U.Vector (Int,Int) -> (Int, Int, Int, Int)
bounds = U.foldl' (\(!r,!l,!u,!d) (x,y) ->
                     let !r' = max r x
                         !l' = min l x
                         !u' = max u y
                         !d' = min d y
                     in (r',l',u',d')) (minBound, maxBound, minBound, maxBound)

data OrInfinity a = Finite a
                  | Infinity
                  deriving (Eq,Ord,Show)

type Time = OrInfinity Rational

-- 有理数の区間ごとに定義された関数を表す型。
-- Piecewise (Rational, Rational) は区分的に1次（以下の）多項式。
-- Piecewise (Rational, Rational, Rational) は区分的に2次（以下の）多項式。
-- いずれも係数を降べきの順に持ったタプルで表す。
type Piecewise a = [(Time,Time,a)]

lift1 :: (Time -> Time -> a -> [(Time,Time,b)]) -> [(Time,Time,a)] -> [(Time,Time,b)]
lift1 f xs = concat
  [ f t0 t1 x
  | (t0,t1,x) <- xs
  ]

lift2 :: (Time -> Time -> a -> a -> [(Time,Time,b)]) -> [(Time,Time,a)] -> [(Time,Time,a)] -> [(Time,Time,b)]
lift2 f xs ys = concat
  [ f s0 s1 x y
  | (t0,t1,x) <- xs
  , (u0,u1,y) <- ys
  , t0 <= u1 && u0 <= t1
  , let s0 = max t0 u0
        s1 = min t1 u1
  ]

simple :: (a -> a -> b) -> [(Time,Time,a)] -> [(Time,Time,a)] -> [(Time,Time,b)]
simple f xs ys =
  [ (s0, s1, f x y)
  | (t0,t1,x) <- xs
  , (u0,u1,y) <- ys
  , t0 <= u1 && u0 <= t1
  , let s0 = max t0 u0
        s1 = min t1 u1
  ]

maxFn :: Piecewise (Rational, Rational) -> Piecewise (Rational, Rational) -> Piecewise (Rational, Rational)
maxFn = lift2 $ \t0 t1 (a,b) (a',b') ->
                  if a == a'
                  then [(t0,t1,(a, max b b'))]
                  else let tx = (b' - b) / (a - a')
                       in if t0 <= Finite tx && Finite tx <= t1
                          then [(t0,Finite tx,min (a, b) (a', b'))
                               ,(Finite tx,t1,max (a, b) (a', b'))
                               ]
                          else case t0 of
                                 Finite t0' -> if (a * t0' + b) < (a' * t0' + b')
                                               then [(t0,t1,(a',b'))]
                                               else [(t0,t1,(a,b))]

minFn :: Piecewise (Rational, Rational) -> Piecewise (Rational, Rational) -> Piecewise (Rational, Rational)
minFn = lift2 $ \t0 t1 (a,b) (a',b') ->
                  if a == a'
                  then [(t0,t1,(a, min b b'))]
                  else let tx = (b' - b) / (a - a')
                       in if t0 <= Finite tx && Finite tx <= t1
                          then [(t0,Finite tx,max (a, b) (a', b'))
                               ,(Finite tx,t1,min (a, b) (a', b'))
                               ]
                          else case t0 of
                                 Finite t0' -> if (a * t0' + b) < (a' * t0' + b')
                                               then [(t0,t1,(a,b))]
                                               else [(t0,t1,(a',b'))]

addFn :: Piecewise (Rational, Rational) -> Piecewise (Rational, Rational) -> Piecewise (Rational, Rational)
addFn = simple (\(a,b) (a',b') -> (a + a', b + b'))
subFn :: Piecewise (Rational, Rational) -> Piecewise (Rational, Rational) -> Piecewise (Rational, Rational)
subFn = simple (\(a,b) (a',b') -> (a - a', b - b'))
mulFn :: Piecewise (Rational, Rational) -> Piecewise (Rational, Rational) -> Piecewise (Rational, Rational, Rational)
mulFn = simple (\(a,b) (a',b') -> (a * a', a * b' + b * a', b * b'))

-- 区間ごとの最小値と、それを実現する引数（デバッグ用）を返す
minimalQ :: Piecewise (Rational, Rational, Rational) -> Piecewise (Rational, Rational)
minimalQ = lift1 $ \t0 t1 (a,b,c) -> [(t0,t1,getMin t0 t1 (a,b,c))]
  where
    getMin (Finite t0) t1 (0,0,c) = (c, t0)
    getMin (Finite t0) (Finite t1) (0,b,c) = min (b * t0 + c, t0) (b * t1 + c, t1)
    getMin (Finite t0) Infinity (0,b,c) | b > 0 = (b * t0 + c, t0)
    getMin _ _ (0,_,_) = error "no minimum"
    getMin (Finite t0) (Finite t1) (a,b,c)
      | a > 0 = let tx = -b/(2*a)
                in (case () of
                     _ | t0 <= tx, tx <= t1 -> ((a * tx + b) * tx + c, tx)
                       | tx < t0            -> ((a * t0 + b) * t0 + c, t0)
                       | otherwise          -> ((a * t1 + b) * t1 + c, t1)
                   )
      | a < 0 = min ((a * t0 + b) * t0 + c, t0)
                    ((a * t1 + b) * t1 + c, t1)
    getMin (Finite t0) Infinity (a,b,c)
      | a > 0 = let tx = -b/(2*a)
                in if t0 <= tx
                   then ((a * tx + b) * tx + c, tx)
                   else ((a * t0 + b) * t0 + c, t0)
      | a < 0 = error "no minimum"

simpleFn :: a -> Piecewise a
simpleFn x = [(Finite 0,Infinity,x)]

main = do
  n <- readLn
  points <- U.replicateM n $ do
    [x',y',d'] <- BS.words <$> BS.getLine
    let Just (x, _) = BS.readInt x'
    let Just (y, _) = BS.readInt y'
    return (x, y, BS.head d')
  let rightwards = U.map (\(x,y,d) -> (x,y)) $ U.filter (\(x,y,d) -> d == 'R') points
      leftwards = U.map (\(x,y,d) -> (x,y)) $ U.filter (\(x,y,d) -> d == 'L') points
      upwards = U.map (\(x,y,d) -> (x,y)) $ U.filter (\(x,y,d) -> d == 'U') points
      downwards = U.map (\(x,y,d) -> (x,y)) $ U.filter (\(x,y,d) -> d == 'D') points
      (rR,lR,uR,dR) = bounds rightwards
      (rL,lL,uL,dL) = bounds leftwards
      (rU,lU,uU,dU) = bounds upwards
      (rD,lD,uD,dD) = bounds downwards
      rF = maxFn (simpleFn (1,fromIntegral rR)) $ maxFn (simpleFn (-1,fromIntegral rL)) (simpleFn (0,fromIntegral $ max rU rD))
      lF = minFn (simpleFn (1,fromIntegral lR)) $ minFn (simpleFn (-1,fromIntegral lL)) (simpleFn (0,fromIntegral $ min lU lD))
      wF = subFn rF lF
      uF = maxFn (simpleFn (1,fromIntegral uU)) $ maxFn (simpleFn (-1,fromIntegral uD)) (simpleFn (0,fromIntegral $ max uR uL))
      dF = minFn (simpleFn (1,fromIntegral dU)) $ minFn (simpleFn (-1,fromIntegral dD)) (simpleFn (0,fromIntegral $ min dR dL))
      hF = subFn uF dF
      targetFn = mulFn wF hF
      (resultQ, tx) = minimum $ map (\(_,_,x) -> x) $ minimalQ targetFn
      result :: Double
      result = fromRational resultQ
  print result

-- 以下、デバッグ用

valueAt :: Rational -> IO (Rational, Rational, Rational, Rational, Rational)
valueAt t = do
  n <- readLn
  points <- replicateM n $ do
    [x',y',d'] <- BS.words <$> BS.getLine
    let Just (x, _) = BS.readInt x'
    let Just (y, _) = BS.readInt y'
    return (fromIntegral x, fromIntegral y, BS.head d')
  let mp = map (\(x,y,d) -> case d of
                     'R' -> (x+t,y)
                     'L' -> (x-t,y)
                     'U' -> (x,y+t)
                     'D' -> (x,y-t)
                 ) points
      xmin = minimum $ map fst mp
      xmax = maximum $ map fst mp
      ymin = minimum $ map snd mp
      ymax = maximum $ map snd mp
  return $ ((xmax - xmin) * (ymax - ymin), xmin, xmax, ymin, ymax)

at :: Rational -> Piecewise a -> a
at x xs = head [ y
               | (t0,t1,y) <- xs
               , t0 <= Finite x && Finite x <= t1
               ]

atL :: Rational -> Piecewise (Rational, Rational) -> Rational
atL x xs = case at x xs of
             (a, b) -> a * x + b
