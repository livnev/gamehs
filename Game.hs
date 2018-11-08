{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Game where

class GameState s where
  showState :: s -> String
  result :: s -> Maybe Result
  whoseMove :: s -> Player

class (GameState s) => Game s p | s -> p where
  move :: s -> p -> Maybe s  
  positions :: s -> [p]

data Player = One | Two
  deriving (Eq, Ord, Show)

otherPlayer :: Player -> Player
otherPlayer One = Two
otherPlayer Two = One

data Draw = Draw ()
  deriving (Eq, Ord, Show)

data Result = Result (Either Player Draw)
  deriving (Eq, Ord, Show)

data Controller s = Controller {
  makeMove :: s -> IO s
  }

playGame :: (Game s p) => (Controller s) -> (Controller s) -> s -> IO Result
playGame c1 c2 gs = do
  let winner = result gs
  case winner of
    Just (Result who) -> return (Result (who))
    Nothing -> do
      let player = whoseMove gs
      let controller = case player of
                         One -> c1
                         Two -> c2
      gs' <- makeMove controller gs
      playGame c1 c2 gs'
