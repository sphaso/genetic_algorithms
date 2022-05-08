{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import Data.Ratio (Ratio)
import System.Random (initStdGen, uniformR, StdGen)
import Control.Monad.State (State, evalState, get, put, replicateM)

type Petri a = State StdGen a
type Population a = [a]

data Chromo a = Chromo { genes :: a, age :: Int, c_fitness :: Ratio Int }

class Chromosome a where
    mkChromosome :: Petri a
    fitness :: a -> Ratio Int
    mutate :: a -> Petri a

class Chromosome a => Generation a where
    mkPopulation :: Petri (Population a)
    evaluate :: Population a -> Population a
    select :: Population a -> [(a, a)]
    crossover :: [(a, a)] -> Petri (Population a)
    evolve :: Ratio Int -> Population a -> Petri (Population a)
    evolve maxFitness pop = do
      let eval = evaluate pop
      if fitness (head eval) >= maxFitness then
        return pop
      else do
        children <- crossover $ select eval
        evolve maxFitness =<< mapM mutate children


