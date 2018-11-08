{-# LANGUAGE MultiParamTypeClasses #-}
module Tic9 where

import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import Data.List (intersperse, elem)
import Data.Maybe (isNothing)

import Game

data Tic9Pos = Tic9Pos (Int, Int)
  deriving (Eq, Ord, Show)

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
                in case peek bs pos of
                     Just x  -> Nothing
                     Nothing -> Just (Tic9State (Tic9Board (Map.insert pos player board)) (otherPlayer player))

boardPositions bs = let all_pos = map int2pos [1..9] in
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

showTic9Board :: Tic9Board -> String
showTic9Board bs = let boardLines = map (\i -> (map (posChar . (peek bs)) [ Tic9Pos (i, 1), Tic9Pos (i, 2), Tic9Pos (i, 3)])) [1, 2, 3] in
  "|" ++ intersperse '|' (boardLines !! 0) ++ "|" ++ "\n"
  ++ "|" ++ intersperse '|' (boardLines !! 1) ++ "|" ++ "\n"
  ++ "|" ++ intersperse '|' (boardLines !! 2) ++ "|" ++ "\n"

posChar :: Maybe Player -> Char
posChar (Just One) = 'o'
posChar (Just Two) = 'x'
posChar Nothing    = ' '

int2coord :: Int -> (Int, Int)
int2coord 1 = (1, 1)
int2coord 2 = (1, 2)
int2coord 3 = (1, 3)
int2coord 4 = (2, 1)
int2coord 5 = (2, 2)
int2coord 6 = (2, 3)
int2coord 7 = (3, 1)
int2coord 8 = (3, 2)
int2coord 9 = (3, 3)

int2pos :: Int -> Tic9Pos
int2pos x = Tic9Pos (int2coord x)

pos2int :: Tic9Pos -> Int
pos2int (Tic9Pos (x, y)) = y + (x-1)*3

genSlices :: [[Tic9Pos]]
genSlices = (fmap . fmap) Tic9Pos [[(1, 1), (1, 2), (1, 3)],
                                   [(2, 1), (2, 2), (2, 3)],
                                   [(3, 1), (3, 2), (3, 3)],
                                   [(1, 1), (2, 1), (3, 1)],
                                   [(1, 2), (2, 2), (3, 2)],
                                   [(1, 3), (2, 3), (3, 3)],
                                   [(1, 1), (2, 2), (3, 3)],
                                   [(1, 3), (2, 2), (3, 1)]]

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
  let m_pos = readMaybe playerMove :: Maybe Int
  case m_pos of
    Nothing -> (putStrLn "Couldn't parse move.") >> humanTic9Input gs
    Just p  -> do
      let pos = (int2pos p)
      return pos
