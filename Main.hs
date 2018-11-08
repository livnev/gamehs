module Main where
import System.Environment
import Distribution.Simple.Utils (lowercase)

import Game
import Tic9
import Tic81
import Controller

answer2Bool :: String -> Maybe Bool
answer2Bool "y"   = Just True
answer2Bool "yes" = Just True
answer2Bool "no"  = Just False
answer2Bool "n"   = Just False
answer2Bool _     = Nothing

resultsDisplay :: Result -> String
resultsDisplay (Result (Left One)) = "o wins."
resultsDisplay (Result (Left Two)) = "x wins."
resultsDisplay (Result (Right _)) = "Game was a draw."

main :: IO ()
main = do
  putStrLn "Start a game of tic-tac-toe? [y/n]"
  play <- (answer2Bool . lowercase) <$> getLine
  case play of
    Nothing    -> putStrLn "Invalid response." >> main
    Just False -> putStrLn "Exiting..."
    Just True  -> do
      putStrLn "Starting new game..."
      result <- playGame humanTic81Controller monte1000Controller initTic81
      putStrLn (resultsDisplay result)
      putStrLn "The game has ended."
      main
