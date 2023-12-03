module Main where

import Data.Char

type MarkedNumber = (Char, Bool)

(!?) :: [a] -> Int -> Maybe a
xs !? i | i >= 0 && i < length xs = Just $ xs !! i
        | otherwise = Nothing

symb :: Char -> Bool
symb c = (c /= '.') && (not $ isDigit c)

cpos :: [Char] -> [Char] -> [Char] -> Int -> [Maybe Char]
cpos p c n i = [c !? (i-1), c !? (i+1), p !? i, n !? i, p !? (i-1), p !? (i+1), n !? (i-1), n !? (i+1)]

mark :: [Char] -> [Char] -> [Char] -> Int -> MarkedNumber
mark p c n i = (c !! i, isDigit (c !! i) && (any (== (Just True)) $ map (fmap symb) $ cpos p c n i))

markParts1 :: [[Char]] -> Int -> [MarkedNumber]
markParts1 xs i
  = let c = xs !! i
        z = replicate (length c) '.'
        p = case (i == 0) of True -> z; False -> xs !! (i-1)
        n = case (i == (length xs) - 1) of True -> z; False -> xs !! (i+1)
    in map (mark p c n) [0..(length c)-1]

markParts :: [[Char]] -> [[MarkedNumber]]
markParts xs = map (markParts1 xs) [0..(length xs)-1]

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy = splitBy' []
  where
    splitBy' acc _ []
      | not $ null acc = [acc]
      | otherwise = []
    splitBy' acc p (x:xs)
      | p x && (not $ null acc) = acc:(splitBy' [] p xs)
      | p x = splitBy' [] p xs
      | otherwise = splitBy' (acc ++ [x]) p xs

extractNumbers1 :: [MarkedNumber] -> [[MarkedNumber]]
extractNumbers1 = splitBy (\(c, _) -> not $ isDigit c)

isPart :: ([Char], [Bool]) -> Bool
isPart (_, bs) = or bs

getParts1 :: [[MarkedNumber]] -> [Integer]
getParts1 xs = map (read . fst) $ filter isPart $ map unzip xs

solve :: [[Char]] -> Integer
solve xs = sum $ concat $ map getParts1 $ map extractNumbers1 $ markParts xs

main :: IO ()
main = (solve <$> lines <$> getContents) >>= print

