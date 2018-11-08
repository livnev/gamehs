module Controller where

import Text.Read (readMaybe)
import System.Random
import Data.Maybe (catMaybes)
import Data.List.Extras.Argmax (argmax)

import Game
import Tic9
import Tic81

randomController :: (Game s p) => Controller s
randomController = Controller randomMove

monte1000Controller :: (Game s p) => Controller s
monte1000Controller = Controller (monteCarloMove 1000)

humanController :: (Game s p) => (s -> IO p) -> Controller s
humanController input = Controller (humanMove input)

humanMove :: (Game s p) => (s -> IO p) -> s -> IO s
humanMove input gs = do
  putStrLn (showState gs)
  pos <- input gs
  let m_gs' = move gs pos
  case m_gs' of
    Nothing  -> putStrLn "Bad move." >> humanMove input gs
    Just gs' -> putStrLn (showState gs') >> return gs'

randomMove :: (Game s p) => s -> IO s
randomMove gs = do
  let options = positions gs
  i <- randomRIO (0, (length options) - 1)
  let pos = options !! i
  let (Just gs') = move gs pos
  return gs'

indmax :: (Ord a) => [a] -> Maybe Int
indmax [] = Nothing
indmax xs = Just (argmax (xs !!) [0..(length xs)-1])

monteCarloMove :: (Game s p) => Int -> s -> IO s
monteCarloMove runs gs = do
  putStrLn "Computer is thinking..."
  let nextMoves = catMaybes (map (move gs) (positions gs))
  scores <- sequence (map (monteCarloScore runs) nextMoves)
  let Just best_i = indmax scores
  let best = nextMoves !! best_i
  return best

mean :: [Int] -> Maybe Double
mean xs = let len    = fromIntegral (length xs)
              sum_xs = fromIntegral (sum xs)
          in if len == 0 then Nothing
             else Just (sum_xs / len)

-- scores from the perspective of the previous mover
-- lifted to IO for randomness
monteCarloScore :: (Game s p) => Int -> s -> IO Double
monteCarloScore runs gs = do
  let player = (otherPlayer . whoseMove) gs
  results <- sequence (map (playGame randomController randomController) (replicate runs gs))
  let scores = map (result2score player) results
  let Just average = mean scores
  return average

result2score :: Player -> Result -> Int
result2score who (Result (Left x))     = if x == who then 5 else 0
result2score who (Result (Right draw)) = 4

-- human Tic9 controller
humanTic9Controller = humanController (humanTic9Input::Tic9State -> IO Tic9Pos)

-- human Tic81 controller
humanTic81Controller = humanController (humanTic81Input::Tic81State -> IO Tic81Pos)
