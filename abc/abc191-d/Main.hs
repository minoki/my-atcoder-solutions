-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BS
import Data.Ratio

main = do
  [cx,cy,r] <- map (read @Double . BS.unpack) . BS.words <$> BS.getLine
  let cx4, cy4, r4 :: Int64
      cx4 = round (cx * 10^4)
      cy4 = round (cy * 10^4)
      r4 = round (r * 10^4)
      isInside x y = (10^4 * x - cx4)^2 + (10^4 * y - cy4)^2 <= r4^2
      minX = ceiling $ (cx4 - r4) % (10^4)
      maxX = floor $ (cx4 + r4) % (10^4)
      go1 !acc !x !yB !yT | x > maxX = acc
                          | otherwise = let yB' = if isInside x yB then
                                                    let go !y | isInside x (y - 1) = go (y - 1)
                                                              | otherwise = y
                                                    in go yB
                                                  else
                                                    let go !y | isInside x (y + 1) = y + 1
                                                              | y > yT = y
                                                              | otherwise = go (y + 1)
                                                    in go yB
                                            yT' = if isInside x yT then
                                                    let go !y | isInside x (y + 1) = go (y + 1)
                                                              | otherwise = y
                                                    in go yT
                                                  else
                                                    let go !y | isInside x (y - 1) = y - 1
                                                              | y < yB = y
                                                              | otherwise = go (y - 1)
                                                    in go yT
                                        in if yT' < yB' then
                                             acc
                                           else
                                             go1 (acc + yT' - yB' + 1) (x + 1) yB' yT'
      y0 = round cy
      go0 !x | x > maxX = 0
             | otherwise = if isInside x y0 then
                             go1 0 x y0 y0
                           else
                             go0 (x + 1)
  print $ go0 minX
