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
-- | A datatype representing the type of the results of processing input files.
module Command.Result
    ( Result (Success, Error)
    , isSuccess
    , isError
    )
  where

-- Internal imports
import Data.Location ( Location )

-- | Result of the global process
data Result a = Success
              | Error a String Location

-- | 'True' if the result is a success, 'False' otherwise.
isSuccess :: Result a -> Bool
isSuccess Success = True
isSuccess _       = False

-- | 'True' if the result is an error, 'False' otherwise.
isError :: Result a -> Bool
isError = not . isSuccess
