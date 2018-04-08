{-# LANGUAGE DeriveGeneric #-}
module GraphSearch where

import Data.Heap as Heap
import GHC.Generics
import Board
import Debug.Trace
import Control.Monad.Writer

type Dist = Node -> Game -> Int
data Game = Game { n :: Int
                 , state :: Board
                 , goal :: Board
                 } deriving (Show, Eq, Generic)

data Node = NullNode 
          | Node { game :: Game
                 , parent :: Node
                 , cost :: Int
                 } deriving (Show, Eq)
instance Ord Node where
    NullNode `compare` NullNode               = EQ
    (Node _ _ x1) `compare` (Node _ _ x2) = x1 `compare` x2
    NullNode `compare` (Node _ _ _)         = LT
    (Node _ _ _) `compare` NullNode         = GT

--Book Keeping, globals to count # of nodes made
myFunc :: Int -> Int -> Writer (Sum Int) Int
myFunc a b = tell (Sum 1) >> return (a + b)

solve :: Game -> Maybe ([Game], Int)
solve game = solver distance game
solver ::Dist -> Game -> Maybe ([Game], Int)
solver dist game
    | isGoal finalGame        = Just ((map toGame (toOrigin finalNode)),nodeCount)
    | otherwise               = Nothing
    where
        firstNode = Node game NullNode fnd
        fnd = dist (Node game NullNode 0) game - 1
        finalGame = toGame finalNode
        heap = Heap.fromList [firstNode] :: MinHeap Node
        (finalNode, nodeCount) = (solver' dist (heap, 0))

solver' :: Dist -> (Heap.MinHeap Node, Int) -> (Node, Int)
solver' dist (heap, nodes)
    | isGoal (game node) = (node, nodes)
    | otherwise          = solver' dist (step dist (heap, (nodes)))
    where
        Just node = Heap.viewHead heap


step :: Dist -> (Heap.MinHeap Node, Int) -> (Heap.MinHeap Node, Int)
step dist (heap, nodes) = ((Heap.drop 1 (toHeap nnl heap)), newNodes)
    where
        Just node = Heap.viewHead heap
        (Node g _ _) = node
        ng = Prelude.filter (notBacktracking node) (successors g)
        nnl = map (nextNode dist node) ng
        newNodes = nodes + (Prelude.length $ successors g)


-- Given a node, make sure it isn't just a reapeat of a game
notBacktracking :: Node -> Game -> Bool
notBacktracking NullNode _ = True
notBacktracking (Node ng np _) game
    | ng == game = False
    | otherwise  = notBacktracking np game
nBTnodecount :: Node -> Int
nBTnodecount NullNode = 0
nBTnodecount (Node _ np _) = 1 + nBTnodecount np

-- Given a game, give all possible successors of that game
successors :: Game -> [Game]
successors (Game n board goal) = map makegame (possibleBoards board)
    where
        makegame = (\ b -> (Game n b goal))

-- Path from start to finish
toOrigin :: Node -> [Node]
toOrigin NullNode = []
toOrigin node = (toOrigin (parent node)) ++ [node]

-- Add list to heap
toHeap [] heap = heap
toHeap (x:xs) heap = toHeap xs (Heap.insert x heap)

-- Take the parent node, and a succesor game to create a child node
nextNode :: (Node -> Game -> Int) -> Node -> Game -> Node
nextNode dist node game = Node game node (dist node game)
    --where x = trace ("Node dist: " ++ show (dist node game)) (dist node game)

distance :: Node -> Game -> Int
distance (Node _ _ x) _ = x + 1

isGoal :: Game -> Bool
isGoal (Game _ board goal) = board == goal

toGame :: Node -> Game
toGame node = game node

