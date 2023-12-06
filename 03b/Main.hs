module Main where

import Data.Char
import Data.Maybe

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

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

extractNumbers1 :: [Char] -> [(Integer, [Integer])]
extractNumbers1 = map (mapFst read) . map unzip . splitBy ((not . isDigit) . fst) . (flip zip) [0..]

extractNumbers :: [[Char]] -> [[(Integer, [(Integer, Integer)])]]
extractNumbers = map (\(rs, i) -> map (mapSnd (map ((,) i))) rs) . map (mapFst extractNumbers1) . (flip zip) [0..]

getStars1 :: [Char] -> [Integer]
getStars1 = map snd . filter ((== '*') . fst) . (flip zip) [0..]

getStars :: [[Char]] -> [[(Integer, Integer)]]
getStars = map (\(rs, i) -> map ((,) i) rs) . map (mapFst getStars1) . (flip zip) [0..]

squareAt :: (Integer, Integer) -> [(Integer, Integer)]
squareAt (y, x)
  = let ys = take 3 [y-1..]
        xs = take 3 [x-1..]
        yss = replicate 3 ys
    in concat $ zipWith (\ys x -> map (,x) ys) yss xs

isAdjacent :: (Integer, Integer) -> (Integer, Integer) -> Bool
isAdjacent p1 p2 = elem p1 $ squareAt p2

starNumbers :: [(Integer, [(Integer, Integer)])] -> (Integer, Integer) -> [Integer]
starNumbers ns s = map fst $ filter (any (isAdjacent s) . snd) ns

asGear :: [(Integer, [(Integer, Integer)])] -> (Integer, Integer) ->  Maybe Integer
asGear ns s
  = let sns = starNumbers ns s
    in if (length sns == 2) then Just $ product sns else Nothing

gears :: [(Integer, [(Integer, Integer)])] -> [(Integer, Integer)] -> [Integer]
gears ns ss = catMaybes $ map (asGear ns) ss

solve :: [[Char]] -> Integer
solve ls
  = let ns = concat $ extractNumbers ls
        ss = concat $ getStars ls
    in sum $ gears ns ss

main :: IO ()
main = (solve <$> lines <$> getContents) >>= print

