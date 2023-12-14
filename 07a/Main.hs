module Main where

import Data.Function
import Data.List
import Data.Maybe

newtype Card = Card Int deriving (Eq, Ord)

cardChars :: [Char]
cardChars = "AKQJT98765432"

card :: Char -> Card
card c
  | Just i <- (elemIndex c cardChars) = Card (length cardChars - i - 1)
  | otherwise = error "invalid card char provided"

instance Show Card where
  show (Card i) = [cardChars !! (length cardChars - i - 1)]

newtype Hand = Hand [Card]

newtype HandType = HandType Int deriving (Eq, Ord, Show)

hand :: [Card] -> Hand
hand cs
  | length cs == 5 = Hand cs
  | otherwise = error "invalid number of cards provided"

hand' :: [Char] -> Hand
hand' = hand . map card

recl :: ([a] -> ([b], [a])) -> [a] -> [[b]]
recl f = fix (\rec xs -> if null xs then [] else let (a,b) = f xs in a:(rec b))

splits :: Hand -> [[Card]]
splits (Hand cs) = recl (\xs -> span (== head xs) xs) $ sort cs

counts :: Hand -> [(Card, Int)]
counts = map (\cs -> (head cs, length cs)) . splits

ranks :: [([Int], HandType)]
ranks = zip [ [1,1,1,1,1], [1,1,1,2], [1,2,2], [1,1,3], [2,3], [1,4], [5] ] $ map HandType [0..]

rank :: Hand -> HandType
rank = fromJust . flip lookup ranks . (sort . map snd . counts)

instance Eq Hand where
  h1@(Hand xs) == h2@(Hand ys) = rank h1 == rank h2 && xs == ys

instance Ord Hand where
  h1@(Hand xs) `compare` h2@(Hand ys) = (rank h1 `compare` rank h2) <> (xs `compare` ys)

instance Show Hand where
  show (Hand xs) = concat $ map show xs

type Bid = Int

parse :: [Char] -> (Hand, Bid)
parse s = let ws = words s in (hand' $ ws !! 0, read $ ws !! 1)

solve :: [(Hand, Bid)] -> Int
solve = sum . zipWith (*) [1..] . map snd . sortBy (compare `on` fst)

main :: IO ()
main = (solve <$> map parse <$> lines <$> getContents) >>= print

