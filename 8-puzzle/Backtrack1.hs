module Backtrack1 (backtrack1, iterdeep) where

import GHC.Generics
import Board
import GraphSearch
import Debug.Trace

type NodeC = Int

bound = 50

--Return Finished Game (or nothing if bound reached)
iterdeep :: Game -> Maybe ([Game], NodeC)
iterdeep g = Just ((map toGame (toOrigin finalNode)), nc)
    where
        Just (finalNode, nc) = iterdeep' 0 0 g

--Iterate depth (recursive), return final node if found
iterdeep' :: NodeC -> Int -> Game -> Maybe (Node, NodeC)
iterdeep' nc b g
    | b > bound        = Nothing
--    | btnode==Nothing  = iterdeep' (nc+1) (b+1) g
    | btn==Nothing     = iterdeep' (newnc) (b+1) g
    | otherwise        = Just (jbtn, newnc)
    where
        firstNode = (Node g NullNode 0)
        (btn, newnc) = backtrack1' (nc) b [firstNode]
        Just jbtn = btn

--Return Finished Game or nothing if fail
backtrack1 :: Game -> Maybe ([Game], NodeC)
backtrack1 g
    | isGoal finalGame = Just ((map toGame (toOrigin finalNode)), nc)
    | otherwise        = Nothing
    where finalGame = game finalNode
          firstNode = (Node g NullNode 0)
          d         = [firstNode]
          (Just finalNode, nc) = backtrack1' 0 bound d

--Depth first search (without backtracking), take a max depth
backtrack1' :: NodeC -> Int -> [Node] -> (Maybe Node, NodeC)
backtrack1' nc b d@(x:_)
    | isGoal (game x)           = (Just x, nc2)
    | deadend (state (game x))  = (Nothing, nc2)--Should be (Nothing) but book keeping
    | length d > b              = (Nothing, nc2)--Should be (Nothing) but book keeping
    | otherwise                 = loop nc2 b d ng
    where nc2 = nc+ (Prelude.length (successors (game x)))
          ng = Prelude.filter (notBacktracking x) (successors (game x))
          btc = (length ng) * (nBTnodecount x)

loop :: NodeC -> Int -> [Node] -> [Game] -> (Maybe Node, NodeC)
loop nc _ _ [] = (Nothing, nc)
loop nc b d@(x:_) (y:ys) = if bk==Nothing
                                then loop nc2 b d ys
                                else path
                            where path = backtrack1' nc b ((Node y x 0) : d)
                                  (bk, nc2) = path


--DEADEND is a predicate true for arguments that are known not to be on a path 
--  to a solution. In this case, the procedure returns the symbol FAIL.
deadend :: Board -> Bool
deadend board = False


