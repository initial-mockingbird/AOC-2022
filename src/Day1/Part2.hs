{-# LANGUAGE DerivingStrategies #-}
module Day1.Part2 where

import qualified Day1.Utils as U
import Data.Functor ((<&>))


-- | Represents an ordered list of at most 3 elements (could be empty)
newtype OrdList a = OList { getList :: [a] }
  deriving stock (Eq, Ord, Show)

-- | Returns the an ordered list with the greatest 3 elements.
instance (Ord a) => Semigroup (OrdList a) where
  OList a <> OList b = case (a,b) of
    (x:xs,y:ys) -> if x >= y then combine x xs (y:ys) else combine y (x:xs) ys
    _           -> make $ a ++ b
    where
      make = OList . take 3
      combine :: Ord a => a -> [a] -> [a] -> OrdList a
      combine x xs ys = make $ x : getList (OList xs <> OList ys)
  
instance (Ord a) => Monoid (OrdList a) where
  mempty = OList []

-- | Just fold using the Greatest 3 monoid we just came up with.
solution' :: IO Int
solution' = U.solution (OList . pure)
  <&> sum
  .   getList

solution :: IO ()
solution 
  =  putStrLn "----------------------"
  >> putStrLn "D1P2."
  >> solution'
  >>= \i -> print i
  