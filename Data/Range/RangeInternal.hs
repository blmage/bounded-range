{-# LANGUAGE Safe #-}

module Data.Range.RangeInternal where

import Control.Applicative ((<|>), liftA2)
import Data.Maybe (catMaybes, maybeToList)
import Safe (predMay, succMay)

import Data.Range.Data
import Data.Range.Spans
import Data.Range.Util

{-
 - The following assumptions must be maintained at the beginning of these internal
 - functions so that we can reason about what we are given.
 -
 - RangeMerge assumptions:
 - * The span ranges will never overlap the bounds.
 - * The span ranges are always sorted in ascending order by the first element.
 - * The lower and upper bounds never overlap in such a way to make it a full range.
 -}
data RangeMerge a = RM
   { largestLowerBound :: Maybe a
   , largestUpperBound :: Maybe a
   , spanRanges :: [(a, a)]
   }
   | FRM
   deriving (Show, Eq)

emptyRangeMerge :: RangeMerge a
emptyRangeMerge = RM Nothing Nothing []

storeRange :: (Ord a) => Range a -> RangeMerge a
storeRange FullRange = FRM
storeRange (LowerBoundRange lower) = emptyRangeMerge { largestLowerBound = Just lower }
storeRange (UpperBoundRange upper) = emptyRangeMerge { largestUpperBound = Just upper }
storeRange (SpanRange x y) = emptyRangeMerge { spanRanges = [(min x y, max x y)] }
storeRange (SingletonRange x) = emptyRangeMerge { spanRanges = [(x, x)] }

storeRanges :: (Ord a, Enum a, Bounded a) => RangeMerge a -> [Range a] -> RangeMerge a
storeRanges start ranges = foldr unionRangeMerges start (map storeRange ranges)

loadRanges :: (Ord a, Enum a, Bounded a) => [Range a] -> RangeMerge a
loadRanges = storeRanges emptyRangeMerge
{-# INLINE[0] loadRanges #-}

exportRangeMerge :: (Ord a, Enum a, Bounded a) => RangeMerge a -> [Range a]
exportRangeMerge FRM = [FullRange]
exportRangeMerge rm = putAll rm
   where
      putAll FRM = [FullRange]
      putAll (RM lb up spans) =
         putUpperBound up ++ putSpans spans ++ putLowerBound lb

      putLowerBound = maybe [] $ pure . simplifyLowerBound
      putUpperBound = maybe [] $ pure . simplifyUpperBound
      putSpans = map simplifySpan

      simplifyLowerBound x
          | x == maxBound = SingletonRange  x
          | otherwise     = LowerBoundRange x

      simplifyUpperBound x
          | x == minBound = SingletonRange  x
          | otherwise     = UpperBoundRange x

      simplifySpan (x, y)
          | x == y        = SingletonRange x
          | x == minBound = simplifyUpperBound y
          | y == maxBound = simplifyLowerBound x
          | otherwise     = SpanRange x y

{-# RULES "load/export" [1] forall x. loadRanges (exportRangeMerge x) = x #-}

intersectSpansRM :: (Ord a) => RangeMerge a -> RangeMerge a -> RangeMerge a
intersectSpansRM one two = RM Nothing Nothing newSpans
   where
      newSpans = intersectSpans (spanRanges one) (spanRanges two)

intersectWith :: (Ord a) => (a -> (a, a) -> Maybe (a, a)) -> Maybe a -> [(a, a)] -> [(a, a)]
intersectWith _ Nothing _ = []
intersectWith fix (Just lower) xs = catMaybes $ fmap (fix lower) xs

fixLower :: (Ord a) => a -> (a, a) -> Maybe (a, a)
fixLower lower (x, y) = if lower <= y
   then Just (max lower x, y)
   else Nothing

fixUpper :: (Ord a) => a -> (a, a) -> Maybe (a, a)
fixUpper upper (x, y) = if x <= upper
   then Just (x, min y upper)
   else Nothing

intersectionRangeMerges :: (Ord a, Enum a, Bounded a) => RangeMerge a -> RangeMerge a -> RangeMerge a
intersectionRangeMerges FRM two = two
intersectionRangeMerges one FRM = one
intersectionRangeMerges one two = RM
   { largestLowerBound = newLowerBound
   , largestUpperBound = newUpperBound
   , spanRanges = joinedSpans
   }
   where
      lowerOneSpans = intersectWith fixLower (largestLowerBound one) (spanRanges two)
      lowerTwoSpans = intersectWith fixLower (largestLowerBound two) (spanRanges one)
      upperOneSpans = intersectWith fixUpper (largestUpperBound one) (spanRanges two)
      upperTwoSpans = intersectWith fixUpper (largestUpperBound two) (spanRanges one)
      intersectedSpans = intersectSpans (spanRanges one) (spanRanges two)

      sortedResults = foldr1 insertionSortSpans
         [ lowerOneSpans
         , lowerTwoSpans
         , upperOneSpans
         , upperTwoSpans
         , intersectedSpans
         , calculateBoundOverlap one two
         ]

      joinedSpans = joinSpans . unionSpans $ sortedResults

      newLowerBound = calculateNewBound largestLowerBound max one two
      newUpperBound = calculateNewBound largestUpperBound min one two

      calculateNewBound
         :: (Ord a)
         => (RangeMerge a -> Maybe a)
         -> (a -> a -> a)
         -> RangeMerge a -> RangeMerge a -> Maybe a
      calculateNewBound ext comp one two = case (ext one, ext two) of
         (Just x, Just y) -> Just $ comp x y
         (_, Nothing) -> Nothing
         (Nothing, _) -> Nothing

calculateBoundOverlap :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a -> [(a, a)]
calculateBoundOverlap one two = catMaybes [oneWay, secondWay]
   where
      oneWay = case (largestLowerBound one, largestUpperBound two) of
         (Just x, Just y) -> if y >= x
            then Just (x, y)
            else Nothing
         _ -> Nothing

      secondWay = case (largestLowerBound two, largestUpperBound one) of
         (Just x, Just y) -> if y >= x
            then Just (x, y)
            else Nothing
         _ -> Nothing

unionRangeMerges :: (Ord a, Enum a, Bounded a) => RangeMerge a -> RangeMerge a -> RangeMerge a
unionRangeMerges FRM _ = FRM
unionRangeMerges _ FRM = FRM
unionRangeMerges one two = fullCheck filterTwo
   where
      filterOne = foldr filterLowerBound boundedRM joinedSpans
      filterTwo = foldr filterUpperBound (filterOne { spanRanges = [] }) (spanRanges filterOne)

      fullCheck :: (Ord a, Enum a, Bounded a) => RangeMerge a -> RangeMerge a
      fullCheck FRM = FRM
      fullCheck rm@(RM (Just x) (Just y) _) = if maybe True (x <=) $ succMay y
         then FRM
         else rm
      fullCheck rm = rm

      newLowerBound = calculateNewBound largestLowerBound min one two
      newUpperBound = calculateNewBound largestUpperBound max one two

      sortedSpans = insertionSortSpans (spanRanges one) (spanRanges two)
      joinedSpans = joinSpans . unionSpans $ sortedSpans

      boundedRM = RM
         { largestLowerBound = newLowerBound
         , largestUpperBound = newUpperBound
         , spanRanges = []
         }

      calculateNewBound
         :: (Ord a)
         => (RangeMerge a -> Maybe a)
         -> (a -> a -> a)
         -> RangeMerge a -> RangeMerge a -> Maybe a
      calculateNewBound ext comp one two = case (ext one, ext two) of
         (Just x, Just y) -> Just $ comp x y
         (z, Nothing) -> z
         (Nothing, z) -> z

filterLowerBound :: (Ord a, Enum a, Bounded a) => (a, a) -> RangeMerge a -> RangeMerge a
filterLowerBound _ FRM = FRM
filterLowerBound a rm@(RM Nothing _ _) = rm { spanRanges = a : spanRanges rm }
filterLowerBound s@(lower, _) rm@(RM (Just lowestBound) _ _) =
   case boundCmp lowestBound s of
      GT -> rm { spanRanges = s : spanRanges rm }
      LT -> rm
      EQ -> rm { largestLowerBound = Just $ min lowestBound lower }

filterUpperBound :: (Ord a, Enum a, Bounded a) => (a, a) -> RangeMerge a -> RangeMerge a
filterUpperBound _ FRM = FRM
filterUpperBound a rm@(RM _ Nothing _) = rm { spanRanges = a : spanRanges rm }
filterUpperBound s@(_, upper) rm@(RM _ (Just upperBound) _) =
   case boundCmp upperBound s of
      LT -> rm { spanRanges = s : spanRanges rm }
      GT -> rm
      EQ -> rm { largestUpperBound = Just $ max upperBound upper }

boundCmp :: (Ord a, Enum a, Bounded a) => a -> (a, a) -> Ordering
boundCmp x (a, b) = if isBetween (Just x) (predMay a, succMay b <|> Just maxBound)
   then EQ
   else if Just x < predMay a then LT else GT

appendSpanRM :: (Ord a, Enum a) => (a, a) -> RangeMerge a -> RangeMerge a
appendSpanRM _ FRM = FRM
appendSpanRM sp@(lower, higher) rm =
   if (newUpper, newLower) == (lub, llb) && isLower lower newLower && (Just higher) > newUpper
      then newRangesRM
         { spanRanges = sp : spanRanges rm
         }
      else newRangesRM
         { spanRanges = spanRanges rm
         }
   where
      newRangesRM = rm
         { largestLowerBound = newLower
         , largestUpperBound = newUpper
         }

      isLower :: Ord a => a -> Maybe a -> Bool
      isLower _ Nothing = True
      isLower y (Just x) = y < x

      lub = largestUpperBound rm
      llb = largestLowerBound rm

      newLower = do
         bound <- llb
         return $ if bound <= higher
            then min bound lower
            else bound

      newUpper = do
         bound <- lub
         return $ if lower <= bound
            then max bound higher
            else bound

invertRM :: (Ord a, Enum a, Bounded a) => RangeMerge a -> RangeMerge a
invertRM FRM = emptyRangeMerge
invertRM (RM lower upper []) = case (lower >>= predMay, upper >>= succMay) of
    (Nothing, Nothing) -> FRM
    (Just lower', Just upper') -> RM Nothing Nothing [(upper', lower')]
    (lower', upper') -> RM upper' lower' []
invertRM rm = RM
   { largestUpperBound = newUpperBound
   , largestLowerBound = newLowerBound
   , spanRanges = upperSpan ++ betweenSpans ++ lowerSpan
   }
   where
      newLowerValue = succMay . snd . last . spanRanges $ rm
      newUpperValue = predMay . fst . head . spanRanges $ rm

      newUpperBound = case largestUpperBound rm of
         Just _  -> Nothing
         Nothing -> newUpperValue

      newLowerBound = case largestLowerBound rm of
         Just _  -> Nothing
         Nothing -> newLowerValue

      upperSpan = maybeToList $ liftA2 (,) (largestUpperBound rm >>= succMay) newUpperValue
      lowerSpan = maybeToList $ liftA2 (,) newLowerValue (largestLowerBound rm >>= predMay)
      betweenSpans = invertSpans . spanRanges $ rm
