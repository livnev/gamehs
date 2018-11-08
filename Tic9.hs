{-# LANGUAGE MultiParamTypeClasses #-}
module Tic9 where

import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import Data.List (intersperse, elem)
import Data.Maybe (isNothing)

import Game

data Tic9Pos = Tic9Pos Int
  deriving (Eq, Ord)

instance Show Tic9Pos where
  show (Tic9Pos x) = show x

data Tic9Board = Tic9Board {
  boardMap :: Map.Map Tic9Pos Player
  }

data Tic9State = Tic9State Tic9Board Player

boardState :: Tic9State -> Tic9Board
boardState (Tic9State bs _) = bs

instance GameState Tic9State where
  whoseMove (Tic9State _ who) = who
  showState gs = let bs = boardState gs in showTic9Board bs
  result gs = let bs = boardState gs in resultBoard bs

instance Game Tic9State Tic9Pos where
  positions gs = let bs = boardState gs in boardPositions bs
  move gs pos = let player = whoseMove gs
                    bs     = boardState gs
                    board  = boardMap bs
                    new_board = Tic9Board (Map.insert pos player board)
                in case peek bs pos of
                     Just x  -> Nothing
                     Nothing -> Just (Tic9State new_board (otherPlayer player))

boardPositions bs = let all_pos = map Tic9Pos [1..9] in
                      filter (isNothing . (peek bs)) all_pos

resultBoard :: Tic9Board -> Maybe Result
resultBoard bs = let full   = checkFull bs
                     slices = sliceShowBoard bs in
                   if elem "ooo" slices
                   then Just (Result (Left One))
                   else if elem "xxx" slices
                        then Just (Result (Left Two))
                        else if full then
                               Just (Result (Right (Draw ())))
                             else
                               Nothing


peek :: Tic9Board -> Tic9Pos -> Maybe Player
peek bs pos = let board = boardMap bs in
                Map.lookup pos board

initTic9 :: Tic9State
initTic9 = Tic9State (Tic9Board Map.empty) One

makeTic9Row :: Int -> [Tic9Pos]
makeTic9Row i = map (Tic9Pos . (+i*3)) [1,2,3]

showTic9Board :: Tic9Board -> String
showTic9Board bs = let boardLines = map (\i -> (map (posChar . (peek bs)) (makeTic9Row i))) [0, 1, 2] in
  "|" ++ intersperse '|' (boardLines !! 0) ++ "|" ++ "\n"
  ++ "|" ++ intersperse '|' (boardLines !! 1) ++ "|" ++ "\n"
  ++ "|" ++ intersperse '|' (boardLines !! 2) ++ "|" ++ "\n"

posChar :: Maybe Player -> Char
posChar (Just One) = 'o'
posChar (Just Two) = 'x'
posChar Nothing    = ' '

genSlices :: [[Tic9Pos]]
genSlices = (fmap . fmap) Tic9Pos [[1, 2, 3],
                                   [4, 5, 6],
                                   [7, 8, 9],
                                   [1, 4, 7],
                                   [2, 5, 8],
                                   [3, 6, 9],
                                   [1, 5, 9],
                                   [3, 5, 7]]

getSlice :: Tic9Board -> [Tic9Pos] -> [Char]
getSlice bs []         = ""
getSlice bs (pos : ps) = posChar (peek bs pos) : getSlice bs ps

sliceShowBoard :: Tic9Board -> [[Char]]
sliceShowBoard bs = (getSlice bs) <$> genSlices

checkFull :: Tic9Board -> Bool
checkFull bs = case boardPositions bs of
                 [] -> True
                 _  -> False

-- human input for controller
humanTic9Input :: (Game s p) => s -> IO Tic9Pos
humanTic9Input gs = do
  putStrLn ("Human player, enter your move: [1-9]")
  playerMove <- getLine
  let m_pos = inputLine2Pos playerMove
  case m_pos of
    Nothing -> (putStrLn "Couldn't parse move.") >> humanTic9Input gs
    Just pos  -> do
      return pos

inputLine2Pos :: String -> Maybe Tic9Pos
inputLine2Pos playerMove = let m_pos = readMaybe playerMove :: Maybe Int in case m_pos of
  Nothing -> Nothing
  Just p  -> if (0 > p) || (p > 9) then Nothing else Just (Tic9Pos p)
