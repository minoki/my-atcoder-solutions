{-# LANGUAGE BangPatterns #-}
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.Unsafe
import Control.Monad.ST
import Control.Monad.State.Strict

asSTUArray :: ST s (STUArray s i a) -> ST s (STUArray s i a)
asSTUArray = id

main = do
  [h,w] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  -- 1 <= h <= 1000, 1 <= w <= 1000
  initialState <- V.replicateM h BS.getLine
  -- for all (i, j), ((a ! i) `BS.index` j) `elem` "#."
  let initArr :: UArray (Int,Int) Bool
      initArr = runSTUArray $ do
        arr <- newArray ((-1,-1),(h,w)) False
        flip V.imapM_ initialState $ \ !i s -> do
          forM_ (zip [0..] (BS.unpack s)) $ \(!j,!c) -> do
            writeArray arr (i,j) (c == '#')
        return arr
      step :: UArray (Int,Int) Bool -> Maybe (UArray (Int,Int) Bool)
      step arr = runST $ flip evalStateT False $ do
        -- state: Falseなマス（白いマス）が残っているか？
        arr' <- lift $ asSTUArray $ newArray ((-1,-1),(h,w)) False
        forM_ [0..h-1] $ \ !i -> do
          forM_ [0..w-1] $ \ !j -> do
            let !b = (arr!(i,j)) || (arr!(i-1,j)) || (arr!(i+1,j)) || (arr!(i,j-1)) || (arr!(i,j+1))
            lift $ writeArray arr' (i,j) b
            unless b $ put True
        s <- get
        if s
          then Just <$> lift (unsafeFreeze arr')
          else return Nothing
      loop :: Int -> UArray (Int,Int) Bool -> Int
      loop !i !arr = case step arr of
        Just arr' -> loop (i+1) arr'
        Nothing -> i
  if all (\((i,j),b) -> b || i == -1 || i == h || j == -1 || j == w) $ (assocs initArr)
    then putStrLn "0"
    else print $ loop 1 initArr
