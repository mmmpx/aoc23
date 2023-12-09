module Main where

import Data.Either
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

type RangeMap = (Int, Int, Int)
type CategoryMap = [RangeMap]
type Range = (Int, Int)
type Almanac = ([Range], [CategoryMap])

-- Parsing

number  :: Parser Int
number = read <$> many1 digit

rangeMap :: Parser RangeMap
rangeMap = (\x y z -> (x, y, z)) <$> number <*> (spaces *> number) <*> (spaces *> number)

categoryMap :: Parser CategoryMap
categoryMap = (manyTill anyChar (char ':') *> spaces) *> rangeMap `sepEndBy1` endOfLine

range :: Parser Range
range = (,) <$> number <*> (spaces *> number)

input :: Parser [Range]
input = (manyTill anyChar (char ':') *> spaces) *> range `endBy1` spaces

almanac :: Parser Almanac
almanac = (,) <$> input <*> many1 categoryMap

-- Solution

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust ((Just x):_) = Just x
firstJust (_:xs) = firstJust xs

inRange :: Int -> Range -> Bool
inRange x (rf, rl) = x >= rf && x < rf + rl

inRanges :: Int -> [Range] -> Bool
inRanges x rs = any (inRange x) rs

mapRangeR :: Int -> RangeMap -> Maybe Int
mapRangeR x (rt, rf, rl)
  | inRange x (rt, rl) = Just $ rf + (x - rt)
  | otherwise = Nothing

mapCategoryR :: CategoryMap -> Int -> Int
mapCategoryR c x = fromMaybe x $ firstJust $ map (mapRangeR x) c

solve1R :: [CategoryMap] -> Int -> Int
solve1R cs x = foldr mapCategoryR x cs

solveR :: Almanac -> Int
solveR (rs, cs) = head $ filter (flip inRanges rs . solve1R cs) [0..]

main :: IO ()
main = (solveR <$> fromRight undefined <$> parse almanac "" <$> getContents) >>= print

