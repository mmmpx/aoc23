module Main where

import Data.Either
import Data.HashMap.Strict (HashMap, fromList, (!))
import Text.Parsec
import Text.Parsec.String

data Direction = L | R deriving (Eq, Show)

type Node = [Char]
type NNode = (Node, (Node, Node))

type NMap = ([Direction], [NNode])

-- Parsing

lr :: Parser Direction
lr
  = (char 'L' >> return L) <|>
    (char 'R' >> return R)

path :: Parser [Direction]
path = many1 lr

node :: Parser Node
node = many1 (noneOf "()=, ")

pair :: Parser (Node, Node)
pair = (,) <$> (char '(' *> node) <*> (char ',' *> spaces *> node <* char ')')

nnode :: Parser NNode
nnode = (,) <$> node <*> (spaces *> char '=' *> spaces *> pair)

nnodes :: Parser [NNode]
nnodes = sepEndBy1 nnode endOfLine

nmap :: Parser NMap
nmap = (,) <$> path <*> (spaces *> nnodes)

-- Solution

dir :: Direction -> ((a, a) -> a)
dir L = fst
dir R = snd

next :: Direction -> HashMap Node (Node, Node) -> Node -> Node
next d h src = (dir d) $ h ! src

solve :: (Node -> Bool) -> [Direction] -> HashMap Node (Node, Node) -> Node -> Int
solve _ [] _ _ = error "no direction given"
solve dstp (d:ds) h src
  | dstp src = 0
  | otherwise = 1 + (solve dstp ds h (next d h src))

solve' :: NMap -> Int
solve' (ds, ns)
  = let srcp = endsWith 'A'
        dstp = endsWith 'Z'
        src = filter srcp $ map fst ns
        ds' = concat $ repeat ds
        h = fromList ns
    in foldr lcm 1 $ map (solve dstp ds' h) src

endsWith :: Eq a => a -> [a] -> Bool
endsWith x xs = (last xs) == x

main :: IO ()
main = (solve' <$> fromRight undefined <$> parse nmap "" <$> getContents) >>= print
