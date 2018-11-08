{-# LANGUAGE MultiParamTypeClasses #-}
module Tic81 where

import System.Console.Pretty
import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Map (map)
import Data.Maybe (isJust, isNothing)
import Control.Monad (join)

import Tic9
import Game

data Tic81Board = Tic81Board {
  boardMap :: Map.Map Tic9Pos Tic9Board
  }

-- now need to keep track of which sector comes next
data Tic81State = Tic81State Tic81Board Player (Maybe Tic9Pos)

data Tic81Pos = Tic81Pos (Either Tic9Pos (Tic9Pos, Tic9Pos))

asTic81Pos :: Tic9Pos -> Tic81Pos
asTic81Pos x = Tic81Pos (Left x)

boardState :: Tic81State -> Tic81Board
boardState (Tic81State bs _ _) = bs

instance GameState Tic81State where
  whoseMove (Tic81State _ who _) = who
  showState gs = let Tic81State bs _ _ = gs
                     rows = map (\i -> blockConcat (map (\p -> (showTic9Board (peek81 bs p))) (makeTic9Row i))) [0, 1, 2]
                     in intercalate "\n" rows
  result gs = let Tic81State bs81 _ _ = gs
                  bs9 = quotient81Board bs81
                  quotientResult = resultBoard bs9
              in case quotientResult of
                   Just (Result who) -> Just (Result who)
                   Nothing -> if (length (positions gs) == 0)
                              then Just (Result (Right (Draw ())))
                              else Nothing

instance Game Tic81State Tic81Pos where
  positions gs = let Tic81State bs player m_sector = gs
                 in case m_sector of
                      Just sector -> let subbs = peek81 bs sector
                                     in map asTic81Pos (positions (Tic9State subbs player))
                      Nothing -> let bs9 = quotient81Board bs
                                     sectors = positions (Tic9State bs9 player)
                                     poss = map (\sector -> positions (Tic9State (peek81 bs sector) player)) sectors
                                 in map (\xy -> Tic81Pos (Right xy)) (tupleZip sectors poss)
  move gs pos = let Tic81State bs player m_sector = gs
                in case m_sector of
                     Just sector -> case pos of
                                      Tic81Pos (Right _) -> Nothing
                                      Tic81Pos (Left p) -> move81 gs (sector, p)
                     Nothing -> case pos of
                                  Tic81Pos (Left _) -> Nothing
                                  Tic81Pos (Right (sector, p)) -> move81 gs (sector, p)


move81 :: Tic81State -> (Tic9Pos, Tic9Pos) -> Maybe Tic81State
move81 gs (sector, p) = let Tic81State bs player _ = gs
                            subbs = peek81 bs sector
                            m_subgs' = move (Tic9State subbs player) p
                        in case m_subgs' of
                             Nothing -> Nothing
                             Just subgs' -> let (Tic9State subbs' _) = subgs'
                                                p_bs' = if sector == p then subbs' else peek81 bs p
                                                next = if isNothing (resultBoard p_bs') then Just p else Nothing
                                            in
                               Just (Tic81State (Tic81Board (Map.adjust (\x -> subbs') sector (Tic81.boardMap bs))) (otherPlayer player) next)

tupleZip :: [a] -> [[b]] -> [(a, b)]
tupleZip xs ys = join (tupleDistribute (zip xs ys))

tupleDistribute :: [(a, [b])] -> [[(a, b)]]
tupleDistribute [] = []
tupleDistribute ((x, ys):xys) = (map (\y -> (x, y)) ys) : (tupleDistribute xys)                                 

quotient81Board :: Tic81Board -> Tic9Board
quotient81Board (Tic81Board board) = let res2player (Just (Result (Left who))) = Just who
                                         res2player (Just (Result (Right (Draw _)))) = Nothing
                                         res2player Nothing = Nothing
                                     in Tic9Board (Map.mapMaybe (res2player . resultBoard) board) where 

initTic81 :: Tic81State
initTic81 = let ps  = (positions initTic9)
                kvs = zip ps (replicate (length ps) (Tic9Board Map.empty))
  in Tic81State (Tic81Board (Map.fromList kvs)) One Nothing

peek81 :: Tic81Board -> Tic9Pos -> Tic9Board
peek81 bs pos = let board = Tic81.boardMap bs
                in case Map.lookup pos board of
                     Just x -> x
                     Nothing -> (Tic9Board Map.empty)

padTo :: Int -> a -> [a] -> [a]
padTo n x xs = let len = length xs in
               if len >= n then xs
               else let padding = replicate (n - len) x in
                 xs ++ padding

padLines :: [String] -> [String]
padLines ss = let maxlen = maximum (map length ss) in
                map (padTo maxlen ' ') ss

blockJoin :: String -> String -> String
blockJoin s [] = s
blockJoin s t  = let s_split  = (splitOn "\n" s)
                     t_split  = (splitOn "\n" t)
                     s_padded = padLines s_split
                     concatLines = map (\(x, y) -> x ++ y) (zip s_padded t_split)
                  in intercalate "\n" concatLines

blockConcat :: [String] -> String
-- blockConcat [] = []
-- blockConcat (s:ss) = blockJoin s (blockConcat ss)
blockConcat = foldr blockJoin ""

-- human input for controller
-- an extension/wrapper around humanTic9Input
humanTic81Input :: Tic81State -> IO Tic81Pos
humanTic81Input gs = do
  let Tic81State bs player m_sector = gs
  case m_sector of
    Just sector -> do
      putStrLn ("Playing in square " ++ (show sector))
      pos <- humanTic9Input gs
      return (asTic81Pos pos)
    Nothing -> do
      putStrLn ("Human player, choose a square to play in: [1-9]")
      playerMove <- getLine
      let m_sec = inputLine2Pos playerMove
      case m_sec of
        Nothing -> (putStrLn "Couldn't parse move.") >> humanTic81Input gs
        Just sec -> do
          pos <- humanTic9Input (Tic81State bs player (Just sec))
          return (Tic81Pos (Right (sec, pos)))
