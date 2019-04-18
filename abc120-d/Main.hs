{-# LANGUAGE BangPatterns #-}
import Data.Int
import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.ByteString.Char8 as BS

solve :: Int -> Int -> V.Vector (Int,Int) -> V.Vector Int64
solve n m edges = V.create $ do
  -- m 番目の島から順番に島同士が「繋がっていく」と考える

  -- j = assocs ! i : i 番目の島の親が j
  -- assocs <- VM.new n
  -- forM_ [0..n-1] $ \i -> VM.write assocs i i
  assocs <- V.thaw $ V.enumFromN 0 n

  -- numberOfElements ! i : i番目の島と同じ連結成分に属する島の個数
  numberOfElements <- VM.replicate n 1

  -- rank <- VM.replicate n (0 :: Int)

  let getRoot !i = do
        !j <- VM.read assocs i
        if i == j
          then return i
          else do k <- getRoot j
                  VM.write assocs i k
                  return k
     {-
      getRoot !i xs = do
        !j <- VM.read assocs i
        if i == j
          then forM_ xs (\l -> VM.write assocs l i) >> return i
          else getRoot j (i:xs)
     -}
      -- i 番目の島と j 番目の島を繋いで、新たに繋がった組み合わせの個数を返す
      unify !i !j = do
        !i' <- getRoot i -- []
        !j' <- getRoot j -- []
        if i' == j'
          then return 0
          else do
          -- !ri <- VM.read rank i'
          -- !rj <- VM.read rank j'
          let !k = min i' j'
          {-
          !k <- case compare ri rj of
                  EQ -> do VM.write rank i' (ri + 1)
                           return i'
                  LT -> return j'
                  GT -> return i'
          -}
          VM.write assocs i' k
          VM.write assocs j' k
          !n1 <- VM.read numberOfElements i'
          !n2 <- VM.read numberOfElements j'
          VM.write numberOfElements k (n1 + n2)
          return $! n1 * n2
  result <- VM.new m
  VM.write result (m-1) $ fromIntegral n * (fromIntegral n - 1) `quot` 2
  forM_ [m-1,m-2..1] $ \i -> do
    let (a,b) = edges V.! i
    convenience <- unify a b
    prevInconveniences <- VM.read result i
    VM.write result (i-1) (prevInconveniences - convenience)
  return result

readIntPair :: IO (Int, Int)
readIntPair = do
  [a,b] <- BS.words <$> BS.getLine
  case (BS.readInt a, BS.readInt b) of
    (Just (a', _), Just (b', _)) -> return (a',b')

main = do
  (n,m) <- readIntPair -- 2 <= n <= 10^5, 1 <= m <= 10^5
  edges <- V.replicateM m $ do
    (a,b) <- readIntPair
    return (a-1, b-1)
  -- maxIncovenience < n * (n - 1) / 2 < 10^10 (2^32 < 10^10)
  let result = solve n m edges
  V.forM_ result print
