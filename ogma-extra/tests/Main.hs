-- Copyright 2020 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY
-- OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT
-- LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO
-- SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
-- PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE
-- SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF
-- PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN
-- ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR
-- RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR
-- ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE. FURTHER,
-- GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING
-- THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES
-- IT "AS IS."
--
-- Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST
-- THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS
-- ANY PRIOR RECIPIENT. IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN
-- ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE,
-- INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S
-- USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE
-- UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
-- PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW. RECIPIENT'S SOLE REMEDY
-- FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS
-- AGREEMENT.
--
-- | Test ogma-extra internals.
module Main where

-- External imports
import Data.Either                          ( isLeft, isRight )
import Test.Framework                       ( Test, defaultMainWithOpts )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck                      ( Property, (==>), Fun (Fun) )

-- Internal imports
import Data.List.Extra ( headEither, toHead, toTail )

-- | Run all unit tests on Ogma's core.
main :: IO ()
main =
  defaultMainWithOpts tests mempty

-- | All unit tests for Ogma's core.
tests :: [Test.Framework.Test]
tests =
  [ testProperty "Data.List.Extra.toHead (diag commutativity)" propToHead
  , testProperty "Data.List.Extra.toHead (identity)"           propToHeadId
  , testProperty "Data.List.Extra.toHead (composition)"        propToHeadCmp
  , testProperty "Data.List.Extra.toHead (length)"             propToHeadSameLength
  , testProperty "Data.List.Extra.toTail (diag commutativity)" propToTail
  , testProperty "Data.List.Extra.toTail (identity)"           propToTailId
  , testProperty "Data.List.Extra.toTail (composition)"        propToTailCmp
  , testProperty "Data.List.Extra.toTail (length)"             propToTailSameLength
  , testProperty "Data.List.Extra.headEither (Right case)"     propHeadEitherRight
  , testProperty "Data.List.Extra.headEither (Left case)"      propHeadEitherLeft
  ]

-- * Data.List.Extra

-- | Test that 'toHead' applies a transformation to the head of a list.
propToHead :: [Int] -> Fun Int Int -> Property
propToHead xs (Fun _ f) = not (null xs) ==> f (head xs) == head (toHead f xs)

-- | Test that 'toHead' preserves the identity.
propToHeadId :: [Int] -> Bool
propToHeadId xs = toHead id xs == xs

-- | Test that 'toHead' preserves function composition.
propToHeadCmp :: [Int] -> Fun Int Int -> Fun Int Int -> Bool
propToHeadCmp xs (Fun _ f) (Fun _ g) = toHead (f . g) xs == toHead f (toHead g xs)

-- | Test that 'toHead' does not alter the length of the list.
propToHeadSameLength :: [Int] -> Fun Int Int -> Bool
propToHeadSameLength xs (Fun _ f) = length xs == length (toHead f xs)

-- | Test that 'toTail' applies a transformation to the tail of a list.
propToTail :: [Int] -> Fun Int Int -> Property
propToTail xs (Fun _ f) = not (null xs) ==> fmap f (tail xs) == tail (toTail f xs)

-- | Test that 'toTail' preserves the identity.
propToTailId :: [Int] -> Bool
propToTailId xs = toTail id xs == xs

-- | Test that 'toHead' preserves function composition.
propToTailCmp :: [Int] -> Fun Int Int -> Fun Int Int -> Bool
propToTailCmp xs (Fun _ f) (Fun _ g) = toTail (f . g) xs == toTail f (toTail g xs)

-- | Test that 'toTail' does not alter the length of the list.
propToTailSameLength :: [Int] -> Fun Int Int -> Bool
propToTailSameLength xs (Fun _ f) = length xs == length (toTail f xs)

-- | Test that 'headEither' returns a 'Right' value if applied to a non-empty
-- list.
propHeadEitherRight :: [Int] -> Property
propHeadEitherRight xs = not (null xs) ==> isRight (headEither xs)

-- | Test that 'headEither' returns a 'Left' value if applied to an empty list.
propHeadEitherLeft :: [Int] -> Property
propHeadEitherLeft xs = null xs ==> isLeft (headEither xs)
