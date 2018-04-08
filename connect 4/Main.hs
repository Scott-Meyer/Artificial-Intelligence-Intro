{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import System.Random
import System.IO
import Data.List
import Debug.Trace

type Grid = [[Int]]
type Player = Int
data Connect4 = Connect4 { grid :: Grid
                         , height :: Int
                         , width :: Int
                         , player :: Player
                         } deriving (Show, Generic)
instance FromJSON Connect4
data Move = Move { move :: Int
                 , mplayer :: Player
                 } deriving (Show, Generic, Eq, Ord)
instance ToJSON Move where
    toJSON p = object [ "move" .= move p ]
data Node = NullNode
          | Node { nMove :: Move
                 , parent :: Node
          } deriving (Show, Eq, Ord)

tg = Connect4 [[0,0,0,0,0,0]
              ,[0,0,0,0,0,0]
              ,[0,0,0,0,0,2]
              ,[0,0,0,0,2,1]
              ,[0,0,0,0,0,1]
              ,[0,0,0,0,0,1]
              ,[0,0,0,0,0,0]] 6 7 2

tg2 = Connect4 [[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,1],[2,1,2,1,2,1],[0,0,0,0,0,1],[0,0,0,0,0,2],[0,0,0,0,0,0]] 6 7 2

parseJ :: B.ByteString -> (Either String Connect4)
parseJ json = (eitherDecode json) :: (Either String Connect4)

validMoves :: Connect4 -> [Move]
validMoves g = toPos (player g) 0 (map head (grid g))

toPos :: Player -> Int -> [Int] -> [Move]
toPos _ _ [] = []
toPos p i (x:xs) = if (x == 0)
                    then (Move i p) : rest
                    else rest
    where rest = toPos p (i+1) xs

minMax :: Int -> Connect4 -> Move
minMax depth c4 = (snd (ratemove !! (length ratemove `div` 2)))
    where
        maxRate = minimum sucRatings
        ratemove = filter (\x -> fst x == maxRate) (zip sucRatings sucMoves)
        sucRatings = trace ("\n"++ (show (map (nega.(toNode NullNode)) sucMoves))) (map (nega.(toNode NullNode)) sucMoves)
        sucMoves = validMoves c4
        nega = negamax c4 (depth - 1) (-9999) 9999 (-1)

negamax :: Connect4 -> Int -> Int -> Int -> Int -> Node -> Int
negamax c4 depth alpha beta color node
    | depth == 0 = color * boardRating
    | sucMoves == [] = (color * boardRating)
    | gameOver c4 = color * boardRating
    | otherwise  = bestValue
    where
      boardRating = (boardRate (curBoard c4 node))
      bestValue = bestVal (-9999 : v) (-9999) alpha beta
      v = map (negate.nega.(toNode node)) sucMoves
      nega = negamax c4 (depth - 1) (-beta) (-alpha) (-color)
      sucMoves = if (color == 1)
                    then validMoves c4
                    else validMoves (otherPlayer c4)

bestVal :: [Int] -> Int -> Int -> Int -> Int
bestVal (x:xs) bv a b
    | alpha >= b = bestValue
    | xs == []   = bestValue
    | otherwise  = bestVal xs bestValue alpha b
    where
        bestValue = maximum [bv, x]
        alpha = maximum [a, x]

huristic :: [Int] -> Int
huristic boardRates = if (maximum boardRates == 4)
                        then 999999
                        else sum $ filter (== (maximum boardRates)) boardRates

toNode :: Node -> Move -> Node
toNode n m = (Node m n)

curBoard :: Connect4 -> Node -> Connect4
curBoard g n = foldr applyMove g (movePath n)

--Given a node, return the move path end->stard down to that node
movePath :: Node -> [Move]
movePath (Node m NullNode) = m : []
movePath (Node m p) = m : movePath p

otherPlayer :: Connect4 -> Connect4
otherPlayer (Connect4 g h w p) = (Connect4 g h w player)
    where player = if (p == 1) then 2 else 1

-- Given a move, and a board, return the resulting board
applyMove :: Move -> Connect4 -> Connect4
applyMove m (Connect4 g h w p) = (Connect4 (makeGrid m g) h w p)

makeGrid :: Move -> Grid -> Grid
makeGrid _ [] = []
makeGrid (Move 0 p) (x:xs) = makeCol p x : makeGrid (Move (-1) p) xs
makeGrid (Move i p) (x:xs) = x : makeGrid (Move (i-1) p) xs

makeCol :: Int -> [Int] -> [Int]
makeCol p (x:[]) = p : []
makeCol p (x:xs@(y:_))
    | y /= 0 = p : xs
    | otherwise = x : makeCol p xs

gameOver :: Connect4 -> Bool
gameOver c4 = or [gamerate == 9999, gamerate == -9999]
    where gamerate = boardRate c4

-- Rate a board
boardRate :: Connect4 -> Int
boardRate (Connect4 g _ _ p)
    | friendly == 4 = 9999
    | enemy == 4    = -9999
    | otherwise     = friendly
    where
        friendly = maximum (map (inrow p) rg)
        enemy = maximum (map (inrow ep) rg)
        rg = rotated g
        ep = if (p == 1) then 2 else 1

rotated :: Grid -> [Grid]
rotated g = [g
            , rowM g
            , filter size (diagonals (rowM g))
            , filter size (diagonals (reverse (rowM g)))]
    where size = (\ x -> length x > 3)
inrow :: Player -> Grid -> Int
inrow p m 
    | p `notElem` (concat m) = 0
    | otherwise = maximum (map maximum (filter (/= []) (map ((map length).(filter (elem p)).group) m)))

rowM :: Grid -> Grid
rowM ls@(x:_) = [map (!! i) ls | i <- [0..(length x)-1]]

-- diagonals taken from stack overflow
-- http://stackoverflow.com/questions/32465776/getting-all-the-diagonals-of-a-matrix-in-haskell
diagonals []       = []
diagonals ([]:xss) = xss
diagonals xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                                  ([]:(diagonals (map tail xss)))

getMove :: Connect4 -> Move
getMove c4 = if (player c4 `notElem` (concat $ grid c4))
                then (Move (width c4 `div` 2) (player c4))
                else minMax 4 c4


-- BEGIN UNSAFE IO
-- Main running of program

putOut :: String -> IO ()
putOut str = do
    hPutStrLn stderr str
    putStrLn str
    hFlush stdout

play :: IO ()
play = do
    js <- getLine
    hPutStrLn stderr js
    case (parseJ $ B.pack js) of
        Left err -> hPutStrLn stderr err
        Right mygame -> do
            putOut $ B.unpack (encode (getMove mygame))
            play

main :: IO ()
main = hPutStrLn stderr "Connect Four" >> play
