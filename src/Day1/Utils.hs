{-# LANGUAGE OverloadedStrings #-}
module Day1.Utils 
  ( readGroup
  , readGroups
  , solution
  ) where


import Data.Attoparsec.ByteString ( Parser, count, sepBy, sepBy1 ) 
import Data.Attoparsec.ByteString.Char8 ( endOfLine, decimal )
import System.IO.Streams.File ( withFileAsInput )
import System.IO.Streams.Attoparsec ( parseFromStream )

{-
 Remember that sepBy1 = p (separator p)*,
 and if (separator p)* fails, it backtracks, to right before separator
 even if we are parsing p.
 leaving  the last \n not consumed:

  1234\n
  5678\n <- this \n is not consummed.
  \n
  212\n

-}
readGroup :: Parser [Int]
readGroup = sepBy1 decimal endOfLine 

{-|
  Groups each block by summing the numbers:

  1234
  5678
  23

  1236
  54
  9

  is equal to: [1234+5689+23,1236+54+9]
-}
readGroups :: Parser [Int]
readGroups = fmap sum <$> sepBy readGroup (count 2 endOfLine)

{-|
  Fold the groups using the given monoid.
-}
solution :: Monoid m => (Int -> m) -> IO m
solution m 
  = foldMap m <$> withFileAsInput "inputs/day1" (parseFromStream readGroups)
