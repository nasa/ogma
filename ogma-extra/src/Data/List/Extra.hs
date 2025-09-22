-- Copyright 2020 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at
--
--      https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
--
-- | Auxiliary functions for working with values of type '[]'.
module Data.List.Extra where

-- External imports
import Data.List ( isSuffixOf )

-- | Safely extract the head of a list.
headEither :: [a] -> Either String a
headEither (a:_) = Right a
headEither []    = Left "Empty list"

-- | Apply a transformation only to the head of a list.
toHead :: (a -> a) -> [a] -> [a]
toHead f (x:xs) = f x : xs
toHead _ xs     = xs

-- | Apply a transformation only to the tail of a list.
toTail :: (a -> a) -> [a] -> [a]
toTail f (x:xs) = x : fmap f xs
toTail _ xs     = xs

-- | Remove a suffix from a string, if present.
stripSuffix :: String -> String -> String
stripSuffix suffix string
  | isSuffixOf suffix string = take (length string - length suffix) string
  | otherwise                = string
