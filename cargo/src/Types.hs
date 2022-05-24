{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import System.Random (initStdGen, uniformR, StdGen)
import Control.Monad.State (State, evalState, get, put, replicateM)

type Petri a = State StdGen a
type Population a = [a]

data Chromo a = Chromo { genes :: a, age :: Int, c_fitness :: Int }

class Chromosome a where
    mkChromosome :: Petri a
    fitness :: a -> Int
    mutate :: a -> Petri a

class Chromosome a => Generation a where
    mkPopulation :: Petri (Population a)
    evaluate :: Population a -> Population a
    select :: Population a -> [(a, a)]
    crossover :: [(a, a)] -> Petri (Population a)
    evolve :: (a -> Int) -> Population a -> Petri (Population a)
    evolve age pop = do
      let eval = evaluate pop
      if age (head eval) >= 500000 then
        return pop
      else do
        children <- crossover $ select eval
        evolve age =<< mapM mutate children


