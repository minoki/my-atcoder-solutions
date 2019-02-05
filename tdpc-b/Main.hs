import Control.Monad
import Data.Array.IO
import qualified Data.Vector.Unboxed as V
import System.Exit

parseInts :: String -> [Int]
parseInts s = case reads s of
                [(x,t)] -> x : parseInts t
                _ -> []

revParseInts :: String -> [Int]
revParseInts s = loop s []
  where loop s acc = case reads s of
                       [(x,t)] -> loop t (x : acc)
                       _ -> acc

main = do
  [na,nb] <- parseInts <$> getLine
  as <- (V.fromList . revParseInts) <$> getLine
  bs <- (V.fromList . revParseInts) <$> getLine
  when (V.length as /= na || V.length bs /= nb) $ putStrLn "input error" >> exitFailure
  arr <- newArray ((0, 0), (na, nb)) 0 :: IO (IOUArray (Int,Int) Int)
  forM_ [1 .. na + nb] $ \k -> do
    forM_ [max 0 (k - nb) .. min na k] $ \i -> do
      let j = k - i
      -- 0 <= i <= na, 0 <= j <= nb
      -- (i,j) /= (0,0) because i + j >= 1
      let score = if even (na + nb - k)
                  then (+) -- すぬけ
                  else subtract -- すめけ
      z <- case (i,j) of
             (0, _) -> score (bs V.! (j - 1)) <$> readArray arr (i, j - 1)
             (_, 0) -> score (as V.! (i - 1)) <$> readArray arr (i - 1, j)
             _      -> do x <- score (as V.! (i - 1)) <$> readArray arr (i - 1, j)
                          y <- score (bs V.! (j - 1)) <$> readArray arr (i, j - 1)
                          return $ if even (na + nb - k)
                                   then max x y -- すぬけ
                                   else min x y -- すめけ
      writeArray arr (i, j) z
  v <- readArray arr (na, nb)
  print $ (V.sum as + V.sum bs + v) `quot ` 2
