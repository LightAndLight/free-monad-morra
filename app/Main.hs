module Main where

import Control.Monad.Free
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Monoid
import System.IO
import System.Random

data MorraF a = PlayerTurn (Int -> a)
              | ComputerTurn (Int -> a)
              | PlayerWon a
              | ComputerWon a

instance Functor MorraF where
    fmap g (PlayerTurn f) = PlayerTurn $ g . f
    fmap g (ComputerTurn f) = ComputerTurn $ g . f
    fmap g (PlayerWon a) = PlayerWon $ g a
    fmap g (ComputerWon a) = ComputerWon $ g a

type Morra a = Free MorraF a

playerTurn :: Morra Int
playerTurn = liftF $ PlayerTurn id

computerTurn :: Morra Int
computerTurn = liftF $ ComputerTurn id

data Winner = Player | Computer

playerWon :: Morra ()
playerWon = liftF $ PlayerWon ()

computerWon :: Morra ()
computerWon = liftF $ ComputerWon ()

data Score = Score { playerScore :: Int, computerScore :: Int }

game :: StateT Score (Free MorraF) Winner
game = do
    Score pScore cScore <- get
    p <- lift $ playerTurn
    c <- lift $ computerTurn
    if odd (p + c)
        then do
            modify incPlayerScore
            lift $ playerWon
        else do
            modify incComputerScore
            lift $ computerWon
    game
  where
    incPlayerScore (Score pScore cScore) = Score (pScore + 1) cScore
    incComputerScore (Score pScore cScore) = Score pScore (cScore + 1)

runMorra :: Morra a -> IO a
runMorra = foldFree morraIO
  where
    morraIO (PlayerTurn f) = do
        putStr "P: "
        hFlush stdout
        f . read <$> getLine
    morraIO (ComputerTurn f) = f <$> randomIO
    morraIO (PlayerWon a) = do
        putStrLn "Player won"
        pure a
    morraIO (ComputerWon a) = do
        putStrLn "Computer won"
        pure a

main :: IO Winner 
main = runMorra $ evalStateT game (Score 0 0) 
