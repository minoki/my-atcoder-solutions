{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Control.Monad.Reader
import Data.Int
import Data.List
import Data.Char
import Data.Bits
import qualified Data.IntMap.Strict as M
import qualified Data.Vector.Mutable as V
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
                                | otherwise = (x,i+1,xs) : allOccurrencesNotIn (i+1) xs e

type Memo a = ReaderT (V.IOVector (M.IntMap Integer)) IO a

numberOfSubstringsX :: AlphabetSet -> Int -> String -> Memo Integer
numberOfSubstringsX !e !i [] = return 1
numberOfSubstringsX !e@(AlphabetSet ex) !i s = do
  arr <- ask
  m <- V.read arr i
  case M.lookup ex m of
    Just v -> return v
    Nothing -> do v <- loop e i s
                  -- if v < 0 then traceShow (e,i,s,v) $ error "numberOfSubstringsX: impossible" else return ()
                  V.write arr i (M.insert ex v m)
                  return v
  where
    loop !e !i (x:xs) | x `elemAS` e = numberOfSubstringsX e (i+1) xs
                      | otherwise = liftM2 (+) (numberOfSubstringsX emptyAS (i+1) xs) (numberOfSubstringsX (insertAS x e) (i+1) xs)

lexIndexX :: Integer -> AlphabetSet -> Int -> String -> Memo (Maybe String)
lexIndexX 0 !e !j !s = return (Just "")
lexIndexX !i !e !j !s
  | i > 0 = case allOccurrencesNotIn j s e of
              [] -> return Nothing
              t@(_:_) -> do
                let (x,j',xs) = minimumBy (\(x,_,_) (y,_,_) -> compare x y) t
                n <- numberOfSubstringsX emptyAS j' xs
                if i <= n
                  then do v <- lexIndexX (i - 1) emptyAS j' xs
                          case v of
                            Just t -> return (Just (x:t))
                            Nothing -> traceShow (i,x,xs) $ error "impossible"
                  else lexIndexX (i - n) (insertAS x e) j s
  | otherwise = traceShow (i,e,j,s) $ error "impossible"

-- lexIndexX 0 emptyAS "aba" = Just ""
-- lexIndexX 1 emptyAS "aba" = Just "a"
-- lexIndexX 2 emptyAS "aba" = Just "aa"
-- lexIndexX 3 emptyAS "aba" = Just "ab"
-- lexIndexX 4 emptyAS "aba" = Just "aba"
-- lexIndexX 5 emptyAS "aba" = Just "b"
-- lexIndexX 6 emptyAS "aba" = Just "ba"

main = do
  s <- getLine -- 1 <= length s <= 10^6
  k <- readLn :: IO Integer -- 1 <= k <= 10^18
  arr <- V.replicate (length s + 1) M.empty
  v <- runReaderT (lexIndexX k emptyAS 0 s) arr
  case v of
    Nothing -> putStrLn "Eel"
    Just t -> putStrLn t
