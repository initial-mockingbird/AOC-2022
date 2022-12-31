module Day1.Part1 
  ( solution
  ) where

import qualified Day1.Utils as U (solution)
import Data.Semigroup ( Max(Max) )
import Data.Coerce ( coerce )


-- | In part1 we would like to find the `Max`imum group, thus 
-- `Max` monoid fits perfectly.
solution' :: IO Int
solution' = coerce <$> U.solution Max


solution :: IO ()
solution 
  =  putStrLn "----------------------"
  >> putStrLn "D1P1."
  >> solution'
  >>= \i -> print i
  