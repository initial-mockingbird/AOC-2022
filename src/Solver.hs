module Solver (solveAll) where


import qualified Day1.Part1 as D1P1
import qualified Day1.Part2 as D1P2


solveAll :: IO ()
solveAll = sequence_
  [ D1P1.solution
  , D1P2.solution
  ]
