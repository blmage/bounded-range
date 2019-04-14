{-# LANGUAGE Safe #-}

-- | Nested Ranges are common in practical usage. They appear in such forms as library
-- version numbers ("Version 1.4.5.6" for example).
--
-- It is very useful to be able to compare these ranges to one another. This module exists
-- for the purpose of allowing these comparisons between nested ranges. The module builds
-- upon the basic range concept from other parts of this library.
module Data.Range.NestedRange
  ( NestedRange(..)
  , fromVersion
  , inNestedRange
  ) where

import Data.Range
import Data.Version

-- | The Nested Range is a structure that in a nested form of many ranges where there can
-- be multiple ranges at every level.
--
-- For example, saying that you require a minimum version of 1.2.3 could be represented as:
--
-- @
-- NestedRange [[LowerBoundRange 1],[LowerBoundRange 2],[LowerBoundRange 3]]
-- @
data NestedRange a = NestedRange [[Range a]]
  deriving(Eq, Show)

-- I wanted to know if a nested number of elements are in a given range. That way I can
-- just immediately run a single function and tell things about ranges.

-- | Given a list of nested values and a nested range tell us wether the nested value
-- exists inside the nested range.
--
-- == Examples
--
-- In a simple case:
--
-- @
-- ghci> inNestedRange [2, 8, 3] (NestedRange [[SpanRange 1 2]] :: NestedRange Integer)
-- True
-- (0.01 secs, 558,400 bytes)
-- ghci>
-- @
--
-- Not in the bounds:
--
-- @
-- ghci> inNestedRange [2, 8, 3] (NestedRange [[SpanRange 1 2], [UpperBoundRange 7]] :: NestedRange Integer)
-- False
-- (0.00 secs, 558,896 bytes)
-- ghci>
-- @
--
-- For something based on Data.Version:
--
-- @
-- ghci> version = Version [2, 8, 3] []
-- ghci> upperBound = Version [2, 7] []
-- ghci> inNestedRange (versionBranch version) (fromVersion UpperBoundRange upperBound)
-- False
-- ghci>
-- ghci> inNestedRange (versionBranch version) (fromVersion LowerBoundRange upperBound)
-- True
-- ghci>
-- @
inNestedRange :: Ord a => [a] -> NestedRange a -> Bool
inNestedRange values (NestedRange ranges) = go values ranges
   where
      go :: Ord a => [a] -> [[Range a]] -> Bool
      go [] [] = True -- If there is nothing left then they are equal
      go _  [] = True -- If you have already found the values you have to be in range then they are
      go [] _  = False -- If you have not fully matched it yet then it is not in range.
      go (value : vs) (range : rs) = inRanges range value && go vs rs

-- | This method converts the "Data.Version" datatype into a "NestedRange".
--
-- For example:
--
-- @
-- ghci> fromVersion LowerBoundRange (Version [1, 2, 3] [])
-- NestedRange [[LowerBoundRange 1],[LowerBoundRange 2],[LowerBoundRange 3]]
-- (0.01 secs, 624,736 bytes)
-- ghci>
-- @
fromVersion :: (Int -> Range Int) -> Version -> NestedRange Int
fromVersion bound = NestedRange . fmap (return . bound) . versionBranch