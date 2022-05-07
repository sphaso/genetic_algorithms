module Population (cycleOfLife) where

import Control.Monad.State (State, evalState, get, put, replicateM)
import Data.Function (on)
import Data.List (unfoldr, sortBy, maximumBy)
import Data.Vector (Vector, update, fromList)
import qualified Data.Vector as V
import System.Random (initStdGen, uniformR, StdGen)

type Chromosome = Vector Int
type Population = [Chromosome]
type Petri a = State StdGen a

rand01 :: Petri Int
rand01 = do
  g <- get
  let (i, newG) = uniformR (0 :: Int, 1 :: Int) g
  put newG
  return i

mkChromosome :: Petri Chromosome
mkChromosome = V.replicateM 1000 rand01

mkPopulation :: Petri Population
mkPopulation = replicateM 100 mkChromosome

evaluate :: Population -> Population
evaluate = reverse . sortBy fitnessFunction

select :: Population -> [(Chromosome, Chromosome)]
select [] = []
select (a:b:xs) = (a, b) : select xs

crossover :: (Chromosome, Chromosome) -> Petri (Chromosome, Chromosome)
crossover (a, b) = do
  g <- get
  let (ix, newG) = uniformR (0 :: Int, 999 :: Int) g
  let (ax, ay) = V.splitAt ix a
  let (bx, by) = V.splitAt ix b
  put newG
  return (ax <> by, bx <> ay)

mutation :: Chromosome -> Petri Chromosome
mutation chromosome = do
  g <- get
  case uniformR (0 :: Double, 1 :: Double) g of
    (r, newG) | r < 0.05 -> do
        put newG
        shuffle chromosome
    (_, newG) -> do
        put newG
        return chromosome

generate :: Population -> Petri Population
generate pop =
  concat <$>
      mapM (\pair -> do
          (c1, c2) <- crossover pair
          cx <- mutation c1
          cy <- mutation c2
          pure [c1, c2]
           ) fit
  where
    fit = select $ evaluate pop

cycleOfLife :: IO Int
cycleOfLife = do
    g <- initStdGen
    pure $ evalState go g
  where
    go :: Petri Int
    go = do
      pop <- mkPopulation
      untilM pop

    untilM :: Population -> Petri Int
    untilM pop = do
      x <- generate pop
      let best = sum $ maximumBy fitnessFunction x
      if best > 990 then
        return best
      else
        untilM x

fitnessFunction :: Chromosome -> Chromosome -> Ordering
fitnessFunction = compare `on` sum

-- Shuffling like it's 1970
shuffle :: Chromosome -> Petri Chromosome
shuffle xs = fisherYates xs 0 (length xs)

fisherYates :: Chromosome -> Int -> Int -> Petri Chromosome
fisherYates xs i l
  = if i == l
      then do
        return xs
      else do
        g <- get
        let (ix, newG) = uniformR (0 :: Int, l - 1) g
        put newG
        fisherYates (swap xs i ix) (i + 1) l

swap :: Chromosome -> Int -> Int -> Chromosome
swap xs i j = update xs updV
  where
    a = xs V.! i
    b = xs V.! j
    updV = fromList [(j, a), (i, b)]
