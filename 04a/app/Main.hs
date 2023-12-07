module Main where

import Data.Either
import Text.Parsec
import Text.Parsec.String

type Card = ([Integer], [Integer])

-- Parsing

numbers :: Parser [Integer]
numbers = map read <$> sepEndBy1 (many1 digit) spaces

card :: Parser Card
card = (,) <$> (string "Card" *> spaces *> many1 digit *> string ":" *> spaces *> numbers) <*> (string "|" *> spaces *> numbers)

cards :: Parser [Card]
cards = many1 card

-- Solution

nseq :: [Integer]
nseq = map (2^) [0..]

score :: Card -> Integer
score (w, a) = snd $ last $ (0,0):(zip (filter (flip elem w) a) nseq)

solve :: [Card] -> Integer
solve = sum . map score

main :: IO ()
main = (solve <$> fromRight undefined <$> parse cards "" <$> getContents) >>= print

