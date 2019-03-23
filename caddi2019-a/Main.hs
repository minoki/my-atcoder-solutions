import Data.Int
import Data.List
import Data.Monoid
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

data Point = Point !Int !Int !Int deriving (Eq, Show)
-- coordinates <= 1000

distance :: Point -> Point -> Double
distance (Point x y z) (Point x' y' z') = sqrt $ fromIntegral $ (x - x')^2 + (y - y')^2 + (z - z')^2

main = do
  -- l = 1000, n = 1000, m = 100000
  [l,n,m] <- map (read :: String -> Int) . words <$> getLine
  -- 1 <= ri <= 200, 1 <= pi <= 80000
  rp <- fmap (V.fromListN n) $ sequence $ replicate n $ do
    [r, p] <- words <$> getLine
    return (read r :: Int, read p :: Int64)
  -- 1 <= ai < bi <= n, 1 <= ci <= 600, 1 <= di <= 80000
  abcd <- sequence $ replicate m $ do
    [a, b, c, d] <- words <$> getLine
    let radiusA = fst (rp V.! read a)
        radiusB = fst (rp V.! read b)
    return (read a :: Int, read b :: Int, read c :: Int, read d :: Int64)
  let loc = V.create $ do
        loc <- VM.replicate n (-1,-1,-1)
        let rp_sorted = sortBy (\(i,(r1,p1)) (j,(r2,p2)) -> compare p2 p1 <> compare r1 r2 <> compare i j) $ zip [0..] $ V.toList rp
            loop x ((i,(r,p)):rest)
              | x + 2 * r <= l = VM.write loc i (x + r, r, r) >> loop (x + 2 * r) rest
              | otherwise = loop x rest
            loop x [] = return ()
        loop 0 rp_sorted
        return loc
  V.forM_ loc $ \(x,y,z) -> do
    putStrLn $ concat [show x, " ", show y, " ", show z]
  -- upper bound of score: 80000 * n + 80000 * m <= 80000 * 101000 < maxBound :: Int64
