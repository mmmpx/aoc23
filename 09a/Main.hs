module Main where

diffs :: [Int] -> [Int]
diffs [] = []
diffs [_] = []
diffs (x:y:zs) = (y-x):(diffs (y:zs))

predict :: [Int] -> Int
predict xs
  | all (== 0) xs = 0
  | otherwise = (last xs) + (predict $ diffs xs)

solve :: [[Int]] -> Int
solve = sum . map predict

parse :: [Char] -> [[Int]]
parse = map (map read) . map words . lines

main :: IO ()
main = (solve . parse <$> getContents) >>= print

