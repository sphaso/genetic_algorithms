module Main where

import Control.Monad (void)
import Population

main :: IO ()
main = print =<< cycleOfLife
