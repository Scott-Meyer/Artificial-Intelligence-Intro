{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

--import Data.List
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

--import other project files
import Board
import GraphSearch
import Astar as A
import Backtrack1

--Json conversion instances for the Game type
instance FromJSON Game where
    parseJSON = withObject "game" $ \o -> do
        n <- o .: "n"
        state <- o .: "start"
        goal <- o .: "goal"
        return (Game n state goal)
instance ToJSON Game

--For testing
testGame = (Game 3 [[7,2,4],[5,0,6],[8,3,1]] [[0,1,2],[3,4,5],[6,7,8]])

--load json file
getJSON :: (String -> IO B.ByteString)
getJSON = B.readFile
--Attempt to parse a JSON file into a Game object
jsonToGame :: String -> IO (Either String Game)
jsonToGame file = do
    putStrLn $ "beginning " ++ file
    (eitherDecode <$> getJSON file) :: IO (Either String Game)

--Validate a game (is nXn && contins only [1,2..])
validGame :: Game -> Bool
validGame (Game size start goal) = validBoard size start && validBoard size goal

--Given a JSON game, run it
runJsonGame :: IO (Either String Game) -> IO ()
runJsonGame json = do
    js <- json
    case js of
        Left err -> putStrLn err
        Right myGame -> if (validGame myGame)
                            then runPuzzle myGame
                            else putStrLn "INVALID GAME FROM JSON"

printBoards [] = putStrLn "Done."
printBoards (x:xs) = do
    printBoard x
    printBoards xs

-- Show an int in good form (23456789 = 23,456,789)
showInt :: Int -> String
showInt x
    | x < 999 = show x
    | otherwise = showInt' xst
    where
        xst = show x
        xstlen = Prelude.length xst
showInt' :: String -> String
showInt' st
    | stlen <= 3 = st
    | stlen > 3 = showInt' (take (stlen-3) st) ++ "," ++ (drop (stlen-3) st)
    where
        stlen = Prelude.length st

--Run the puzzle Game
runPuzzle :: Game -> IO ()
runPuzzle myGame = do
    putStrLn $ "\tbacktrack1:"
    putStrLn $ "\t\t"++(showInt bnodeCount)++" Boards Generated."
    putStrLn $ "\t\t"++(showInt (length bsolution -1))++" Move solution found."
    putStrLn $ "\tIterative Deepening: "
    putStrLn $ "\t\t"++(showInt idnodeCount)++" Boards Generated."
    putStrLn $ "\t\t"++(showInt (length idsolution -1))++" Move solution found."
    putStrLn $ "\tGraphSearch: "
    putStrLn $ "\t\t"++(showInt gnodeCount)++" Boards Generated."
    putStrLn $ "\t\t"++(showInt (length gsolution -1))++" Move solution found."
    putStrLn $ "\tA*: "
    putStrLn $ "\t\t"++(showInt anodeCount)++" Boards Generated."
    putStrLn $ "\t\t"++(showInt (length asolution -1))++" Move solution found."
                --printBoard $ last (map state gsolution)
    where 
        Just (bsolution, bnodeCount) = backtrack1 myGame
        Just (idsolution, idnodeCount) = iterdeep myGame
        Just (gsolution, gnodeCount) = GraphSearch.solve myGame
        Just (asolution, anodeCount) = A.solve myGame

--Main driver function.
--Attempt to get JSON, then validate the game from JSON, then runPuzzle
main :: IO ()
main = do 
    runJsonGame $ jsonToGame "1-move.json"
    runJsonGame $ jsonToGame "2-moves.json"
    runJsonGame $ jsonToGame "3-moves.json"
    runJsonGame $ jsonToGame "4-moves.json"
    runJsonGame $ jsonToGame "5-moves.json"
    runJsonGame $ jsonToGame "10-moves.json"
    runJsonGame $ jsonToGame "15-moves.json"
    runJsonGame $ jsonToGame "20-moves.json"
    runJsonGame $ jsonToGame "25-moves.json"
    runJsonGame $ jsonToGame "problem-1.json"
    runJsonGame $ jsonToGame "15-puzzle.json"

