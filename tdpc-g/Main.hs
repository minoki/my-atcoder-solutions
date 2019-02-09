{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Control.Monad.Reader
import Data.Int
import Data.Word
import Data.List
import Data.Char
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as V
import Debug.Trace

newtype AlphabetSet = AlphabetSet Int deriving (Eq)

emptyAS :: AlphabetSet
emptyAS = AlphabetSet 0
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

allOccurrencesNotIn :: Int -> String -> AlphabetSet -> [(Char, Int, String)]
allOccurrencesNotIn !i [] _ = []
allOccurrencesNotIn !i (x:xs) e | x `elemAS` e = allOccurrencesNotIn (i+1) xs e
                                | otherwise = (x,i+1,xs) : allOccurrencesNotIn (i+1) xs (insertAS x e)

type INT = {- Integer -} Word64

satAdd :: INT -> INT -> INT
satAdd !a !b = let !c = a + b
               in if c < a || c < b
                  then maxBound
                  else c

type Memo a = ReaderT (V.IOVector INT) IO a

numberOfSubstrings :: Int -> String -> Memo INT
numberOfSubstrings !i [] = return 1
numberOfSubstrings !i s = do
  arr <- ask
  m <- V.read arr i
  if m /= 0
    then return m
    else do v <- doCalc i s
            V.write arr i v
            return v
              where -- doCalc i s = foldl' satAdd 0 <$> sequence [ numberOfSubstrings j xs | (x,j,xs) <- allOccurrencesNotIn i s emptyAS ]
                    doCalc i s = loop 1 (allOccurrencesNotIn i s emptyAS)
                    loop !acc [] = return acc
                    loop !acc _ | acc == maxBound = return acc
                    loop !acc ((x,j,xs):xss) = do y <- numberOfSubstrings j xs
                                                  loop (satAdd acc y) xss

lexIndexW :: INT -> [(Char, Int, String)] -> Memo (Maybe String)
lexIndexW 0 !_ = return (Just "")
lexIndexW !i [] = return Nothing
lexIndexW !i ((x,j,xs):xss) = do
  n <- numberOfSubstrings j xs
  if i <= n
    then do v <- lexIndexX (i - 1) j xs
            case v of
              Just t -> return (Just (x:t))
              Nothing -> traceShow (i,x,xs) $ error "impossible"
    else lexIndexW (i - n) xss

lexIndexX :: INT -> Int -> String -> Memo (Maybe String)
lexIndexX i j s = lexIndexW i $ sortBy (\(x,_,_) (y,_,_) -> compare x y) $ allOccurrencesNotIn j s emptyAS

-- lexIndexX 0 "aba" = Just ""
-- lexIndexX 1 "aba" = Just "a"
-- lexIndexX 2 "aba" = Just "aa"
-- lexIndexX 3 "aba" = Just "ab"
-- lexIndexX 4 "aba" = Just "aba"
-- lexIndexX 5 "aba" = Just "b"
-- lexIndexX 6 "aba" = Just "ba"

runMemo :: String -> Memo a -> IO a
runMemo s m = do
  arr <- V.new (length s + 1)
  runReaderT m arr

main = do
  s <- getLine -- 1 <= length s <= 10^6
  k <- readLn :: IO INT -- 1 <= k <= 10^18
  v <- runMemo s (lexIndexX k 0 s)
  case v of
    Nothing -> putStrLn "Eel"
    Just t -> putStrLn t
