{-# LANGUAGE Safe #-}

-- | This module provides a simple api to access range functionality. It provides standard
-- set operations on ranges, the ability to merge ranges together and, importantly, the ability
-- to check if a value is within a range.
--
-- __Note:__ It is intended that you will read the documentation in this module from top to bottom.
module Data.Range (
      -- * Data types
      Range(..),
      -- * Comparison functions
      inRange,
      inRanges,
      aboveRange,
      aboveRanges,
      belowRange,
      belowRanges,
      rangesOverlap,
      -- * Set operations
      mergeRanges,
      union,
      intersection,
      difference,
      invert,
      -- * Utility methods
      fromRanges
   ) where

import Data.Range.Data
import Data.Range.Util
import qualified Data.Range.Algebra as Alg

-- | Performs a set union between the two input ranges and returns the resultant set of
-- ranges.
--
-- For example:
--
-- @
-- ghci> union [SpanRange 1 10] [SpanRange 5 (15 :: Integer)]
-- [SpanRange 1 15]
-- (0.00 secs, 587,152 bytes)
-- ghci>
-- @
union :: (Ord a, Enum a) => [Range a] -> [Range a] -> [Range a]
union a b = Alg.eval $ Alg.union (Alg.const a) (Alg.const b)
{-# INLINE union #-}

-- | Performs a set intersection between the two input ranges and returns the resultant set of
-- ranges.
--
-- For example:
--
-- @
-- ghci> intersection [SpanRange 1 10] [SpanRange 5 (15 :: Integer)]
-- [SpanRange 5 10]
-- (0.00 secs, 584,616 bytes)
-- ghci>
-- @
intersection :: (Ord a, Enum a) => [Range a] -> [Range a] -> [Range a]
intersection a b = Alg.eval $ Alg.intersection (Alg.const a) (Alg.const b)
{-# INLINE intersection #-}

-- | Performs a set difference between the two input ranges and returns the resultant set of
-- ranges.
--
-- For example:
--
-- @
-- ghci> difference [SpanRange 1 10] [SpanRange 5 (15 :: Integer)]
-- [SpanRange 1 4]
-- (0.00 secs, 590,424 bytes)
-- ghci>
-- @
difference :: (Ord a, Enum a) => [Range a] -> [Range a] -> [Range a]
difference a b = Alg.eval $ Alg.difference (Alg.const a) (Alg.const b)
{-# INLINE difference #-}

-- | An inversion function, given a set of ranges it returns the inverse set of ranges.
--
-- For example:
--
-- @
-- ghci> invert [SpanRange 1 10, SpanRange 15 (20 :: Integer)]
-- [LowerBoundRange 21,UpperBoundRange 0,SpanRange 11 14]
-- (0.00 secs, 623,456 bytes)
-- ghci>
-- @
invert :: (Ord a, Enum a) => [Range a] -> [Range a]
invert = Alg.eval . Alg.invert . Alg.const
{-# INLINE invert #-}

-- | A check to see if two ranges overlap. If they do then true is returned; false
-- otherwise.
rangesOverlap :: (Ord a) => Range a -> Range a -> Bool
rangesOverlap (SingletonRange a) (SingletonRange b) = a == b
rangesOverlap (SingletonRange a) (SpanRange x y) = isBetween a (x, y)
rangesOverlap (SingletonRange a) (LowerBoundRange lower) = lower <= a
rangesOverlap (SingletonRange a) (UpperBoundRange upper) = a <= upper
rangesOverlap (SpanRange x y) (SpanRange a b) = isBetween x (a, b) || isBetween a (x, y)
rangesOverlap (SpanRange _ y) (LowerBoundRange lower) = lower <= y
rangesOverlap (SpanRange x _) (UpperBoundRange upper) = x <= upper
rangesOverlap (LowerBoundRange _) (LowerBoundRange _) = True
rangesOverlap (LowerBoundRange x) (UpperBoundRange y) = x <= y
rangesOverlap (UpperBoundRange _) (UpperBoundRange _) = True
rangesOverlap InfiniteRange _ = True
rangesOverlap a b = rangesOverlap b a

-- | Given a range and a value it will tell you wether or not the value is in the range.
-- Remember that all ranges are inclusive.
--
-- The primary value of this library is performance and this method can be used to show
-- this quite clearly. For example, you can try and approximate basic range functionality
-- with "Data.List.elem" so we can generate an apples to apples comparison in GHCi:
--
-- @
-- ghci> :set +s
-- ghci> elem (10000000 :: Integer) [1..10000000]
-- True
-- (0.26 secs, 720,556,888 bytes)
-- ghci> inRange (SpanRange 1 10000000) (10000000 :: Integer)
-- True
-- (0.00 secs, 557,656 bytes)
-- ghci>
-- @
--
-- As you can see, this function is significantly more performant, in both speed and memory,
-- than using the elem function.
inRange :: (Ord a) => Range a -> a -> Bool
inRange (SingletonRange a) value = value == a
inRange (SpanRange x y) value = isBetween value (x, y)
inRange (LowerBoundRange lower) value = lower <= value
inRange (UpperBoundRange upper) value = value <= upper
inRange InfiniteRange _ = True

-- | Given a list of ranges this function tells you if a value is in any of those ranges.
-- This is especially useful for more complex ranges.
inRanges :: (Ord a) => [Range a] -> a -> Bool
inRanges rs a = any (`inRange` a) rs

-- | Checks if the value provided is above (or greater than) the biggest value in
-- the given range.
--
-- The "LowerBoundRange" and the "InfiniteRange" will always
-- cause this method to return False because you can't have a value
-- higher than them since they are both infinite in the positive
-- direction.
--
-- @
-- ghci> aboveRange (SingletonRange 5) (6 :: Integer)
-- True
-- ghci> aboveRange (SpanRange 1 5) (6 :: Integer)
-- True
-- ghci> aboveRange (SpanRange 1 5) (0 :: Integer)
-- False
-- ghci> aboveRange (LowerBoundRange 0) (6 :: Integer)
-- False
-- ghci> aboveRange (UpperBoundRange 0) (6 :: Integer)
-- True
-- ghci> aboveRange (InfiniteRange) (6 :: Integer)
-- False
-- ghci>
-- @
aboveRange :: (Ord a) => Range a -> a -> Bool
aboveRange (SingletonRange a)       value = value > a
aboveRange (SpanRange x y)          value = value > y
aboveRange (LowerBoundRange _)      _     = False
aboveRange (UpperBoundRange upper)  value = value > upper
aboveRange InfiniteRange            _     = False

-- | Checks if the value provided is above all of the ranges provided.
aboveRanges :: (Ord a) => [Range a] -> a -> Bool
aboveRanges rs a = all (`aboveRange` a) rs

-- | Checks if the value provided is below (or less than) the smallest value in
-- the given range.
--
-- The "UpperBoundRange" and the "InfiniteRange" will always
-- cause this method to return False because you can't have a value
-- lower than them since they are both infinite in the negative
-- direction.
--
-- @
-- ghci> belowRange (SingletonRange 5) (4 :: Integer)
-- True
-- ghci> belowRange (SpanRange 1 5) (0 :: Integer)
-- True
-- ghci> belowRange (SpanRange 1 5) (6 :: Integer)
-- False
-- ghci> belowRange (LowerBoundRange 6) (0 :: Integer)
-- True
-- ghci> belowRange (UpperBoundRange 6) (0 :: Integer)
-- False
-- ghci> belowRange (InfiniteRange) (6 :: Integer)
-- False
-- ghci>
-- @
belowRange :: (Ord a) => Range a -> a -> Bool
belowRange (SingletonRange a)       value = value < a
belowRange (SpanRange x y)          value = value < x
belowRange (LowerBoundRange lower)  value = value < lower
belowRange (UpperBoundRange _)      _     = False
belowRange InfiniteRange            _     = False

-- | Checks if the value provided is below all of the ranges provided.
belowRanges :: (Ord a) => [Range a] -> a -> Bool
belowRanges rs a = all (`belowRange` a) rs

-- | An array of ranges may have overlaps; this function will collapse that array into as few
-- Ranges as possible. For example:
--
-- @
-- ghci> mergeRanges [LowerBoundRange 12, SpanRange 1 10, SpanRange 5 (15 :: Integer)]
-- [LowerBoundRange 1]
-- (0.01 secs, 588,968 bytes)
-- ghci>
-- @
--
-- As you can see, the mergeRanges method collapsed multiple ranges into a single range that
-- still covers the same surface area.
--
-- This may be useful for a few use cases:
--
--  * You are hyper concerned about performance and want to have the minimum number of ranges
--    for comparison in the inRanges function.
--  * You wish to display ranges to a human and want to show the minimum number of ranges to
--    avoid having to make people perform those calculations themselves.
--
-- Please note that the use of any of the operations on sets of ranges like invert, union and
-- intersection will have the same behaviour as mergeRanges as a side effect. So, for example,
-- this is redundant:
--
-- @
-- mergeRanges . union []
-- @
mergeRanges :: (Ord a, Enum a) => [Range a] -> [Range a]
mergeRanges = Alg.eval . Alg.union (Alg.const []) . Alg.const
{-# INLINE mergeRanges #-}

-- | Instantiate all of the values in a range.
--
-- __Warning__: This method is meant as a convenience method, it is not efficient.
--
-- A set of ranges represents a collection of real values without actually instantiating
-- those values. Not instantiating ranges, allows the range library to support infinite
-- ranges and be super performant.
--
-- However, sometimes you actually want to get the values that your range represents, or even
-- get a sample set of the values. This function generates as many of the values that belong
-- to your range as you like.
--
-- Because ranges can be infinite, it is highly recommended to combine this method with something like
-- "Data.List.take" to avoid an infinite recursion.
--
-- == Examples
--
-- A simple span:
--
-- @
-- ghci> take 5 . fromRanges $ [SpanRange 1 10 :: Range Integer]
-- [1,2,3,4,5]
-- (0.01 secs, 566,016 bytes)
-- ghci>
-- @
--
-- An infinite range:
--
-- @
-- ghci> take 5 . fromRanges $ [InfiniteRange :: Range Integer]
-- [0,1,-1,2,-2]
-- (0.00 secs, 566,752 bytes)
-- ghci>
-- @
fromRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromRanges = takeEvenly . fmap fromRange . mergeRanges
   where
      fromRange range = case range of
         SingletonRange x -> [x]
         SpanRange a b -> [a..b]
         LowerBoundRange x -> iterate succ x
         UpperBoundRange x -> iterate pred x
         InfiniteRange -> zero : takeEvenly [tail $ iterate succ zero, tail $ iterate pred zero]
            where
               zero = toEnum 0
