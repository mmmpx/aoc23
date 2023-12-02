module Main where

import Control.Applicative
import Data.Char

firstLast :: [Char] -> Integer
firstLast xs = read [head xs, last xs]

numbers :: [([Char], Char)]
numbers =
  [ ("one", '1')
  , ("two", '2')
  , ("three", '3')
  , ("four", '4')
  , ("five", '5')
  , ("six", '6')
  , ("seven", '7')
  , ("eight", '8')
  , ("nine", '9') ]

tryMatch :: [Char] -> ([Char], a) -> Maybe (a, [Char])
tryMatch xs ("", r) = Just (r, xs)
tryMatch (x:xs) ((y:ys), r)
  | x == y = tryMatch xs (ys, r)
  | otherwise = Nothing
tryMatch _ _ = Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (<|>) Nothing

getNumbers :: [Char] -> [Char]
getNumbers [] = []
getNumbers (x:xs) = case (firstJust $ map (tryMatch (x:xs)) numbers) of
  (Just (r, ys)) -> r:(getNumbers xs) -- xs here instead of ys to handle e.g. "oneight"
  Nothing -> x:(getNumbers xs)

solveOne :: [Char] -> Integer
solveOne = firstLast . filter isDigit . getNumbers

solve :: [[Char]] -> Integer
solve = sum . map solveOne

main :: IO ()
main = (solve <$> lines <$> getContents) >>= print

