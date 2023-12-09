module Main where

import Data.Either
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

type RangeMap = (Int, Int, Int)
type CategoryMap = [RangeMap]
type Almanac = ([Int], [CategoryMap])

-- Parsing

number  :: Parser Int
number = read <$> many1 digit

rangeMap :: Parser RangeMap
rangeMap = (\x y z -> (x, y, z)) <$> number <*> (spaces *> number) <*> (spaces *> number)

categoryMap :: Parser CategoryMap
categoryMap = (manyTill anyChar (char ':') *> spaces) *> rangeMap `sepEndBy1` endOfLine

input :: Parser [Int]
input = (manyTill anyChar (char ':') *> spaces) *> number `endBy1` spaces

almanac :: Parser Almanac
almanac = (,) <$> input <*> many1 categoryMap

-- Solution

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust ((Just x):_) = Just x
firstJust (_:xs) = firstJust xs

inRange :: Int -> RangeMap -> Bool
inRange x (_, rf, rl) = x >= rf && x < rf + rl

mapRange :: Int -> RangeMap -> Maybe Int
mapRange x r@(rt, rf, _)
  | inRange x r = Just $ rt + (x - rf)
  | otherwise = Nothing

mapCategory :: CategoryMap -> Int -> Int
mapCategory c x = fromMaybe x $ firstJust $ map (mapRange x) c

solve1 :: [CategoryMap] -> Int -> Int
solve1 cs x = foldl (flip mapCategory) x cs

solve :: Almanac -> Int
solve (xs, cs) = minimum $ map (solve1 cs) xs

main :: IO ()
main = (solve <$> fromRight undefined <$> parse almanac "" <$> getContents) >>= print

