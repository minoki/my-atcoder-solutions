{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Int
import qualified Data.Set as S
--import Debug.Trace
import Data.List

traceShow _ x = x

pairs :: S.Set String -> [(Char, S.Set String)]
pairs set = case S.minView set of
              Just ("", set') -> pairs set'
              Just (a:as, set') -> let (xs, ys) = S.spanAntitone (\(x:xs) -> x == a) set'
                                   in (a, S.insert as (S.map tail xs)) : pairs ys
              Nothing -> []

-- f (S.fromList (tails s)) : the number of substrings of s, including the empty string
numberOfSubstrings :: S.Set String -> Int64
numberOfSubstrings set
  | S.null set = 0
  | otherwise = 1 + sum [ numberOfSubstrings xss
                        | (x,xs) <- pairs set
                                    {-, traceShow (x,xs) True -}
                        , let xss = S.fromList $ concatMap tails $ S.toList xs
                        ]

-- numberOfSubstrings (S.fromList (tails "aba")) == 7
-- numberOfSubstrings (S.fromList (tails "eel")) == 6

-- set: closed w.r.t. tail (for all x:xs in set, xs is also in set)
lexIndexOf :: Int64 -> S.Set String -> Maybe String
lexIndexOf 0 set | S.null set = Nothing
                 | otherwise = Just ""
lexIndexOf !i set = case S.minView set of
             Just ("", set') -> lexIndexOf i set'
             Just (a:as, set') -> let (xs, ys) = S.spanAntitone (\(x:_) -> x == a) set'
                                      set'' = S.insert as (S.map tail xs)
                                      xss = S.fromList $ concatMap tails $ S.toList set''
                                      m = numberOfSubstrings xss
                                  in traceShow (i,a,set'',m) $ if i <= m
                                                       then case lexIndexOf (i-1) xss of
                                                              Just zs -> Just (a : zs)
                                                              Nothing -> error "bug"
                                                       else lexIndexOf (i - m) ys
             Nothing -> Nothing

-- lexIndexOf 100 (S.fromList $ tails "lexicographical") == Just "capal"

main = do
  s <- getLine -- 1 <= length s <= 10^6
  k <- readLn :: IO Int64 -- 1 <= k <= 10^18
  case lexIndexOf k $ S.fromList $ tails s of
    Nothing -> putStrLn "Eel"
    Just t -> putStrLn t
