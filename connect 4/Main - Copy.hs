{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import System.Random
import Control.Concurrent

data Connect4 = Connect4 { grid :: [[Int]]
                         , height :: Int
                         , width :: Int
                         , player :: Int
                         } deriving (Show, Generic)
instance FromJSON Connect4
data Move = Move { move :: Int } deriving (Generic)
instance ToJSON Move

p1json = "connect-four-stderr-1.txt"
p2json = "connect-four-stderr-2.txt"

parseJ :: String -> IO (Either String Connect4)
parseJ file = (eitherDecode <$> getJ file) :: IO (Either String Connect4)
getJ :: (String -> IO B.ByteString)
getJ f = do
    file <- B.readFile f
    let fl = B.lines file
    if (length fl < 1)
        then return B.empty
        else return $ last fl

getMove :: Connect4 -> IO Move
getMove (Connect4 _ _ w _) = do
    rand <- randomIO :: IO Int
    return $ (Move (rand `mod` w))

driver :: IO ()
driver = do
    game <- parseJ p1json
    case game of
        Right mygame -> do
            myMove <- getMove mygame
            B.appendFile p1json (encode myMove)
        Left err -> do
            threadDelay 1000
            driver

main :: IO ()
main = do
    putStdLn "Connect Four\n"
    driver
