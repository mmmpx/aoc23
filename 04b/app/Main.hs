module Main where

import Data.Either
import Text.Parsec
import Text.Parsec.String

type Card = (Int, ([Int], [Int]))

-- Parsing

number  :: Parser Int
number = read <$> many1 digit

numbers :: Parser [Int]
numbers = sepEndBy1 number spaces

card :: Parser Card
card = (,) <$> (string "Card" *> spaces *> number) <*> ((,) <$> (string ":" *> spaces *> numbers) <*> (string "|" *> spaces *> numbers))

cards :: Parser [Card]
cards = many1 card

-- Solution

cardIdxs :: Card -> [Int]
cardIdxs (i, (w, a)) = [i..(i-1)+(length $ filter (flip elem w) a)]

score :: [Card] -> [Card] -> Int
score _ [] = 0
score ocs (c:cs) = 1 + (score ocs $ map (ocs !!) $ cardIdxs c) + (score ocs cs)

solve :: [Card] -> Int
solve cs = score cs cs

main :: IO ()
main = (solve <$> fromRight undefined <$> parse cards "" <$> getContents) >>= print
