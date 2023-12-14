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

next :: Node -> Direction -> HashMap Node (Node, Node) -> Node
next src d h = (dir d) $ h ! src

solve :: Node -> Node -> [Direction] -> HashMap Node (Node, Node) -> Int
solve _ _ [] _ = error "no direction given"
solve src dst (d:ds) h
  | src == dst = 0
  | otherwise = 1 + (solve (next src d h) dst ds h)

convert :: NMap -> ([Direction], HashMap Node (Node, Node))
convert (ds, ns) = (concat $ repeat ds, fromList ns)

main :: IO ()
main = (uncurry (solve "AAA" "ZZZ") <$> convert <$> fromRight undefined <$> parse nmap "" <$> getContents) >>= print
