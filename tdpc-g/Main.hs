{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Control.Monad.Reader
import Data.Word
import Data.List
import Data.Char
import Data.Bits
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

newtype AlphabetSet = AlphabetSet Int deriving (Eq)

emptyAS, allAS :: AlphabetSet
emptyAS = AlphabetSet 0
allAS = AlphabetSet (2^26 - 1)
elemAS :: Char -> AlphabetSet -> Bool
elemAS !x (AlphabetSet s) = testBit s (ord x - ord 'a')
insertAS :: Char -> AlphabetSet -> AlphabetSet
insertAS !x (AlphabetSet s) = AlphabetSet (setBit s (ord x - ord 'a'))
fromStringAS :: String -> AlphabetSet
fromStringAS = foldl' (flip insertAS) emptyAS
toStringAS (AlphabetSet s) = loop s 'a'
  where loop 0 !c = ""
        loop s !c | testBit s 0 = c : loop (s `shiftR` 1) (succ c)
                  | otherwise = loop (s `shiftR` 1) (succ c)
instance Show AlphabetSet where
  show = show . toStringAS

allOccurrencesNotIn :: Int -> ByteString -> AlphabetSet -> [(Char, Int, ByteString)]
allOccurrencesNotIn !i !s !e
  = case BS.uncons s of
      Nothing -> []
      Just (x,xs) | e == allAS -> []
                  | x `elemAS` e -> allOccurrencesNotIn (i+1) xs e
                  | otherwise -> let !i' = i+1
                                 in (x,i',xs) : allOccurrencesNotIn i' xs (insertAS x e)

type INT = {- Integer -} Word64

satAdd :: INT -> INT -> INT
satAdd !a !b = let !c = a + b
               in if c < a || c < b
                  then maxBound
                  else c

numberOfSubstringsV :: INT -> ByteString -> V.Vector INT
numberOfSubstringsV !maxI s = V.create $ do
  let ls = BS.length s
  v <- V.new (ls + 1)
  forM_ (reverse $ zip [0..] $ BS.tails s) $ \(i,ss) -> do
    let loop !acc [] = return acc
        loop !acc _ | acc > maxI = return acc
        -- loop !acc _ | acc == maxBound = return acc
        loop !acc ((_,j,xs):xss) = do y <- V.read v j
                                      loop (satAdd acc y) xss
    val <- loop 1 (allOccurrencesNotIn i ss emptyAS)
    V.write v i val
  return v

type Memo a = Reader (V.Vector INT) a

lexIndexW :: INT -> [(Char, Int, ByteString)] -> Memo (Maybe ByteString)
lexIndexW 0 !_ = return (Just BS.empty)
lexIndexW !i [] = return Nothing
lexIndexW !i ((x,j,xs):xss) = do
  n <- asks (V.! j)
  if i <= n
    then do v <- lexIndexX (i - 1) j xs
            case v of
              Just t -> return (Just (BS.cons x t))
              Nothing -> error "impossible"
    else lexIndexW (i - n) xss

lexIndexX :: INT -> Int -> ByteString -> Memo (Maybe ByteString)
lexIndexX i j s = lexIndexW i $ sortBy (\(x,_,_) (y,_,_) -> compare x y) $ allOccurrencesNotIn j s emptyAS

-- lexIndexX 0 0 "aba" = Just ""
-- lexIndexX 1 0 "aba" = Just "a"
-- lexIndexX 2 0 "aba" = Just "aa"
-- lexIndexX 3 0 "aba" = Just "ab"
-- lexIndexX 4 0 "aba" = Just "aba"
-- lexIndexX 5 0 "aba" = Just "b"
-- lexIndexX 6 0 "aba" = Just "ba"

main = do
  s <- BS.getLine -- 1 <= length s <= 10^6
  k <- readLn :: IO INT -- 1 <= k <= 10^18
  let arr = numberOfSubstringsV k s
      v = runReader (lexIndexX k 0 s) arr
  case v of
    Nothing -> putStrLn "Eel"
    Just t -> BS.putStrLn t
