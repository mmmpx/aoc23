module Main where

import Data.Either
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

type RGB = (Integer, Integer, Integer)
type Pull = RGB
type Game = (Integer, [Pull])

red :: Integer -> RGB
red x = (x, 0, 0)

red' :: RGB -> Integer
red' (x, _, _) = x

green :: Integer -> RGB
green x = (0, x, 0)

green' :: RGB -> Integer
green' (_, x, _) = x

blue :: Integer -> RGB
blue x = (0, 0, x)

blue' :: RGB -> Integer
blue' (_, _, x) = x

colors :: [([Char], (Integer -> RGB))]
colors = [("red", red), ("green", green), ("blue", blue)]

addRGB :: RGB -> RGB -> RGB
addRGB (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

sumRGB :: [RGB] -> RGB
sumRGB = foldr addRGB (0, 0, 0)

-- Parsing

color :: Parser (Integer -> RGB)
color = choice $ map (\(s, f) -> string' s >> return f) colors

cubes :: Parser RGB
cubes = flip ($) <$> (read <$> many1 digit <* space) <*> color

pull :: Parser Pull
pull = sumRGB <$> sepBy1 cubes (string ", ")

game :: Parser Game
game = (,) <$> (read <$> (string "Game " *> many1 digit) <* string ": ") <*> (sepBy1 pull (string "; "))

games :: Parser [Game]
games = endBy1 game endOfLine

-- Solution

fewest :: Game -> RGB
fewest (_, ps) = (maximum $ map red' ps, maximum $ map green' ps, maximum $ map blue' ps)

power :: RGB -> Integer
power (r, g, b) = r * g * b

solve :: [Game] -> Integer
solve = sum . map power . map fewest

main :: IO ()
main = (solve <$> fromRight undefined <$> parse games "" <$> getContents) >>= print

