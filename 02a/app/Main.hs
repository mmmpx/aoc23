module Main where

import Data.Either
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

type RGB = (Integer, Integer, Integer)
type Config = RGB
type Pull = RGB
type Game = (Integer, [Pull])

red :: Integer -> RGB
red x = (x, 0, 0)

green :: Integer -> RGB
green x = (0, x, 0)

blue :: Integer -> RGB
blue x = (0, 0, x)

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

isValidPull :: Config -> Pull -> Bool
isValidPull (rm, gm, bm) (r, g, b) = and [r <= rm, g <= gm, b <= bm]

isValidGame :: Config -> Game -> Bool
isValidGame c (n, ps) = and $ map (isValidPull c) ps

solve :: Config -> [Game] -> Integer
solve c gs = sum $ map fst $ filter (isValidGame c) gs

main :: IO ()
main = (solve (12, 13, 14) <$> fromRight undefined <$> parse games "" <$> getContents) >>= print

