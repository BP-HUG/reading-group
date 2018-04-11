module Course where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

rollDie :: State StdGen Int
rollDie = do
  s <- get
  let (i, s') = randomR (1, 6) s
  put s'
  return i
  
rollDie' :: State StdGen Int
rollDie' = state $ randomR (1, 5)

rollDice :: State StdGen [Int]
rollDice = do
  die1 <- rollDie
  die2 <- rollDie
  die3 <- rollDie'
  die4 <- rollDie'
  return [die1, die2, die3, die4]
  
rollManyDice :: Int -> State StdGen [Int]
rollManyDice n = sequence (map (\_ -> rollDie') [1..n]) 



replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:(replicate' (n - 1) x)

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' n x = fmap (replicate' n) x

get' :: State s s
get' = state $ \s -> (s, s)

put' :: State s ()
put' = state $ \s -> ((), s)

modify3 :: (a -> a) -> State a ()
modify3 f = do
  s <- get
  put $ f s
  


