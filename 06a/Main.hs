module Main where

type Race = (Int, Int)

calc :: Int -> Int -> Int
calc b t = b * (t - b)

solve1 :: Race -> Int
solve1 (t, d) = length $ filter (> d) $ map (flip calc t) [0..t]

solve :: [Race] -> Int
solve = product . map solve1

main :: IO ()
main = print $ solve [(59,597), (79,1234), (65,1032), (75,1328)]

