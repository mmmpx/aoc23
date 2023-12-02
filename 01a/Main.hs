module Main where

import Data.Char

firstLast :: [Char] -> Integer
firstLast xs = read [head xs, last xs]

solveOne :: [Char] -> Integer
solveOne = firstLast . filter isDigit

solve :: [[Char]] -> Integer
solve = sum . map solveOne

main :: IO ()
main = (solve <$> lines <$> getContents) >>= print

