{-# LANGUAGE BangPatterns #-}
import Control.Monad (forM_)
import Control.Monad.Reader
import Data.Int (Int64)
import Data.Array.Unboxed (UArray, (!))
import Data.Array.ST (runSTUArray, newArray, readArray, writeArray)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

type StringIndexTable = UArray (Int, Char) Int

-- arr ! (i, ord x - ord 'a') : 文字列の i 番目以降に最初に現れる文字 x のインデックス（現れない場合は -1）
-- となるような2次元配列 arr を返す。
mkStringIndexTable :: ByteString -> StringIndexTable
mkStringIndexTable !s = runSTUArray $ do
  arr <- newArray ((0, 'a'), (BS.length s, 'z')) (-1)
  forM_ [BS.length s - 1, BS.length s - 2 .. 0] $ \i -> do
    forM_ ['a'..'z'] $ \j -> do
      v <- readArray arr (i + 1, j)
      writeArray arr (i, j) v
    let x = BS.index s i
    writeArray arr (i, x) i
  return arr

-- テスト用
runWithStringIndexTable :: Reader (ByteString, StringIndexTable) a -> ByteString -> a
runWithStringIndexTable m str
  = let tbl = mkStringIndexTable str
    in runReader m (str, tbl)

-- 文字列の i 番目以降に出現する文字と、その最初の出現位置の組 (c, j) のリストを返す。
-- リストの順番は、アルファベットの若い順である。
-- >>> runWithStringIndexTable (allOccurrences 0) "hello"
-- [('e',1),('h',0),('l',2),('o',4)]
allOccurrences :: Int -> Reader (ByteString, StringIndexTable) [(Char, Int)]
allOccurrences !i = do
  str <- asks fst
  tbl <- asks snd -- tbl == mkStringIndexTable str
  return $ if i >= BS.length str
           then []
           else [ (c, j)
                | c <- ['a'..'z']
                , let j = tbl ! (i, c)
                , j /= -1
                ]

-- v ! i = 〈文字列の i 番目以降からなるスライス〉の部分文字列の個数
-- となるような配列 v を返す。
-- ただし、値が maxI より大きい場合の計算は適宜打ち切られる（maxI より大きい値が適当に入っている）。
-- >>> runWithStringIndexTable mkNumberOfSubstringsVec "aba"
-- [7,4,2,1]
-- >>> runWithStringIndexTable mkNumberOfSubstringsVec "eel"
-- [6,4,2,1]
mkNumberOfSubstringsVec :: Reader (ByteString, StringIndexTable) (V.Vector Int64)
mkNumberOfSubstringsVec = do
  ctx@(str, tbl) <- ask
  let len = BS.length str
      maxI = 10^18+1 :: Int64
  return $ V.create $ do
    v <- V.new (len + 1)
    forM_ [len, len-1 .. 0] $ \i -> do
      let loop !acc [] = return acc
          loop !acc _ | acc > maxI = return acc -- 値が十分大きくなった場合は計算を打ち切ってオーバーフローを回避する
          loop !acc ((_,j):xss) = do y <- V.read v (j + 1)
                                     if y > maxI
                                       then return y -- 値が十分大きくなった場合はry
                                       else loop (acc + y) xss
      val <- loop 1 $ runReader (allOccurrences i) ctx
      V.write v i val
    return v

-- この問題におけるDPの状態（配列）を持ったモナド
type Context a = Reader ((ByteString, StringIndexTable), V.Vector Int64) a
-- (str, tbl, vec) <- ask
-- tbl == mkStringIndexTable str
-- vec == numberOfSubstringsV tbl maxI str

-- 与えられた集合 S の要素の部分文字列のうち、辞書順で i 番目となるものを返す。
-- S は、文脈で与えられる文字列に対して (最初の一文字, その出現位置) という形の要素からなるリストで、アルファベットの若い順に並んでいる。
-- i が大きいときは Nothing を返す。
lexSearch :: Int64 -> [(Char, Int)] -> Context (Maybe ByteString)
lexSearch 0 !_ = return (Just BS.empty) -- TODO: 空集合に関してこれでいいのか？
lexSearch !_ [] = return Nothing -- 空集合
lexSearch !i ((x,j):xss) = do
  n <- asks ((V.! (j + 1)) . snd) -- x から始まる部分文字列の個数
  if i <= n
    then do v <- lexIndexSlice (j + 1) (i - 1)
            case v of
              Just t -> return (Just (BS.cons x t))
              Nothing -> error "impossible"
    else lexSearch (i - n) xss

-- 〈文字列の j 番目以降からなるスライス〉の部分文字列のうち、辞書順で i 番目となるものを返す。
-- i が大きいときは Nothing を返す。
lexIndexSlice :: Int -> Int64 -> Context (Maybe ByteString)
lexIndexSlice !j !i = do xs <- withReader fst $ allOccurrences j
                         lexSearch i xs

-- 部分文字列のうち、辞書順で i 番目となるものを返す。
-- i が大きいときは Nothing を返す。
--
-- >>> run (mapM lexIndex [0..7]) "aba"
-- [Just "",Just "a",Just "aa",Just "ab",Just "aba",Just "b",Just "ba",Nothing]
-- >>> run (mapM lexIndex [0..6]) "eel"
-- [Just "",Just "e",Just "ee",Just "eel",Just "el",Just "l",Nothing]
-- >>> run (lexIndex 100) "lexicographical"
-- Just "capal"
lexIndex :: Int64 -> Context (Maybe ByteString)
lexIndex = lexIndexSlice 0

run :: Context a -> ByteString -> a
run m str = let tbl = mkStringIndexTable str
                arr = runReader mkNumberOfSubstringsVec (str, tbl)
            in runReader m ((str, tbl), arr)

main = do
  str <- BS.getLine -- 1 <= length str <= 10^6
  k <- readLn :: IO Int64 -- 1 <= k <= 10^18 < 2*10^18 < maxBound = 2^63-1
  case run (lexIndex k) str of
    Nothing -> putStrLn "Eel"
    Just result -> BS.putStrLn result
