{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Int
import Data.List
import Data.Char
import Data.Bits

newtype AlphabetSet = AlphabetSet Word deriving (Eq)

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

allOccurrencesNotIn :: String -> AlphabetSet -> [(Char, String)]
allOccurrencesNotIn [] _ = []
allOccurrencesNotIn (x:xs) e | x `elemAS` e = allOccurrencesNotIn xs e
                             | otherwise = (x,xs) : allOccurrencesNotIn xs e

numberOfSubstringsX :: AlphabetSet -> String -> Int64
numberOfSubstringsX !e [] = 1
numberOfSubstringsX !e (x:xs) | x `elemAS` e = numberOfSubstringsX e xs
                              | otherwise = numberOfSubstringsX emptyAS xs + numberOfSubstringsX (insertAS x e) xs

lexIndexX :: Int64 -> AlphabetSet -> String -> Maybe String
lexIndexX 0 !e !s = Just ""
lexIndexX !i !e !s = case allOccurrencesNotIn s e of
                       [] -> Nothing
                       t@(_:_) -> let (x,xs) = minimumBy (\x y -> compare (fst x) (fst y)) t
                                      n = numberOfSubstringsX emptyAS xs
                                  in if i <= n
                                     then case lexIndexX (i - 1) emptyAS xs of
                                            Just t -> Just (x:t)
                                            Nothing -> error "impossible"
                                     else lexIndexX (i - n) (insertAS x e) s

-- lexIndexX 0 emptyAS "aba" = Just ""
-- lexIndexX 1 emptyAS "aba" = Just "a"
-- lexIndexX 2 emptyAS "aba" = Just "aa"
-- lexIndexX 3 emptyAS "aba" = Just "ab"
-- lexIndexX 4 emptyAS "aba" = Just "aba"
-- lexIndexX 5 emptyAS "aba" = Just "b"
-- lexIndexX 6 emptyAS "aba" = Just "ba"

main = do
  s <- getLine -- 1 <= length s <= 10^6
  k <- readLn :: IO Int64 -- 1 <= k <= 10^18
  case lexIndexX k emptyAS s of
    Nothing -> putStrLn "Eel"
    Just t -> putStrLn t
