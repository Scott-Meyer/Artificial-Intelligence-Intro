module Astar(solve) where

import GHC.Generics
import Board
import GraphSearch hiding (solve)

astar :: Node -> Game -> Int
astar (Node pg _ x) game = trav + 1 + hur
    where trav = if x==0 then 0 else x - pcost
          hur  = huristic game
          pcost = huristic pg

huristic :: Game -> Int
huristic game = sum $ map (tileDist game) [1..size]
    where size = n game * (n game) - 1

tileDist :: Game -> Tile -> Int
tileDist (Game _ state goal) x = posDiff (findTile state x) (findTile goal x)

posDiff :: Position -> Position -> Int
posDiff (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

solve :: Game -> Maybe ([Game], Int)
solve game = solver astar game