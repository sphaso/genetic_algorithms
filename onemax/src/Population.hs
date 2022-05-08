{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Population (cycleOfLife) where

import Control.Monad.State (State, evalState, get, put, replicateM)
import Data.Function (on)
import Data.List (unfoldr, sortBy, maximumBy)
import Data.Vector (Vector, update, fromList)
import qualified Data.Vector as V
import System.Random (initStdGen, uniformR, StdGen)

import Types

type Genes = Vector Int
type C = Chromo Genes

instance Chromosome C where
    mkChromosome = do
        genes <- V.replicateM 1000 rand01
        pure $ Chromo { genes = genes, age = 0, c_fitness = 0 }
    fitness = sum . genes
    mutate chromosome = do
      g <- get
      case uniformR (0 :: Double, 1 :: Double) g of
        (r, newG) | r < 0.05 -> do
            put newG
            shuffle chromosome
        (_, newG) -> do
            put newG
            return chromosome

instance Generation C where
    mkPopulation = replicateM 100 mkChromosome
    evaluate xs =
        reverse
          $ sortBy (compare `on` c_fitness)
          $ map
              (\c@(Chromo {..}) -> c { age = age + 1, c_fitness = fitness c })
              xs
    select [] = []
    select (a:b:xs) = (a, b) : select xs
    crossover pairs =
        concat <$>
        mapM
          (\(a, b) -> do
                g <- get
                let (ix, newG) = uniformR (0 :: Int, 999 :: Int) g
                let (ax, ay) = V.splitAt ix (genes a)
                let (bx, by) = V.splitAt ix (genes b)
                put newG
                return [a { genes = ax <> by}, b { genes = bx <> ay }]
          )
          pairs

maxFitness :: Int
maxFitness = 900

rand01 :: Petri Int
rand01 = do
  g <- get
  let (i, newG) = uniformR (0 :: Int, 1 :: Int) g
  put newG
  return i

cycleOfLife :: IO Int
cycleOfLife = do
    g <- initStdGen
    let bestPop = evalState go g
    pure $ fitness $ head $ evaluate bestPop
  where
    go :: Petri (Population C)
    go = evolve maxFitness =<< mkPopulation

-- Shuffling like it's 1970
shuffle :: C -> Petri C
shuffle c@(Chromo {..}) = do
    newGenes <- fisherYates genes 0 (length genes)
    return $ c { genes = newGenes }

fisherYates :: Genes -> Int -> Int -> Petri Genes
fisherYates xs i l
  = if i == l
      then do
        return xs
      else do
        g <- get
        let (ix, newG) = uniformR (0 :: Int, l - 1) g
        put newG
        fisherYates (swap xs i ix) (i + 1) l

swap :: Genes -> Int -> Int -> Genes
swap xs i j = update xs updV
  where
    a = xs V.! i
    b = xs V.! j
    updV = fromList [(j, a), (i, b)]
