{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

-- | Utilities for constructing assertions.
module TestResources.AssertUtils
       (
         failOnError,
         failOnNothing,
         
         checkMaybes,

         isLeft,
         isRight,
         checkEithers,
       )
       where
  

-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit.Base (Assertion,assertFailure)
import Test.HUnit.Lang (FailureReason(..), formatFailureReason)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


failOnError :: Show e
            => (a -> Assertion)
            -> Either e a
            -> Assertion
failOnError _             (Left err) = assertFailure $
                                       "failOnError: " ++ show err
failOnError assertionOnOk (Right x)  = assertionOnOk x

failOnNothing :: (a -> Assertion)
               -> Maybe a
               -> Assertion
failOnNothing _             Nothing    = assertFailure "Nothing: Expected Just something"
failOnNothing assertionOnJust (Just x) = assertionOnJust x


-------------------------------------------------------------------------------
-- - Maybe -
-------------------------------------------------------------------------------


checkMaybes :: (String -> a -> a -> Assertion)
               -- ^ Assertion on two Just values.
               --
               -- First argument is the header passed to this
               -- function.
            -> (a -> String)
               -- ^ A \"show\" function for values.
            -> String
               -- ^ Header to include in failure messages.
            -> Maybe a
            -- ^ Expected value.
            -> Maybe a
            -- ^ Actual value.
            -> Assertion
checkMaybes _ _       _      Nothing Nothing = return ()
checkMaybes _ showVal header Nothing (Just actual) = assertFailure msg
  where
    msg = header ++ ": expected Nothing" ++ 
          "\n" ++
          "got: Just $ " ++ showVal actual
checkMaybes _ showVal header (Just expected) Nothing = assertFailure msg
  where
    msg = header ++ ": expected Just $ " ++ showVal expected ++
          "\n" ++
          "got: Nothing"
checkMaybes assertOnJusts showVal header (Just expected) (Just actual) =
  assertOnJusts header expected actual


-------------------------------------------------------------------------------
-- - Either -
-------------------------------------------------------------------------------


isLeft :: Show b => Either a b -> Assertion
isLeft (Right y) = assertFailure $ formatFailureReason failure
  where
    failure :: FailureReason
    failure =  ExpectedButGot Nothing "Left <anything>" $ show (Right (show y) :: Either () String)
isLeft _ = pure ()


isRight :: Show a => Either a b -> Assertion
isRight (Left x) = assertFailure $ formatFailureReason failure
  where
    failure :: FailureReason
    failure =  ExpectedButGot Nothing "Right <anything>" $ show (Left (show x) :: Either String ())
isRight _ = pure ()


checkEithers :: (String -> l -> l -> Assertion)
                -- ^ Assertion on two Left values (expected, actual).
                --
                -- First argument is the header passed to this
                -- function.
             -> (String -> r -> r -> Assertion)
                -- ^ assertion on two Right values (expected, actual).
                --
                -- First argument is the header passed to this
                -- function.
             -> (l -> String)
                -- ^ A \"show\" function for Left values.
             -> (r -> String)
                -- ^ A \"show\" function for Right values.
             -> String
                -- ^ Header to include in failure messages.
             -> Either l r
             -- ^ Expected value.
             -> Either l r
             -- ^ Actual value.
             -> Assertion
checkEithers checkLs checkRs showL showR msgHeader (Left expected) (Left actual) =
  checkLs (msgHeader ++ "/Rights") expected actual

checkEithers checkLs checkRs showL showR msgHeader (Right expected) (Right actual) =
  checkRs (msgHeader ++ "/Rights") expected actual

checkEithers checkLs checkRs showL showR msgHeader (Left expected) (Right actual) =
  assertFailure $ 
  msgHeader ++ ": expected is Left $ " ++ showL expected ++
  "\n" ++
  "actual is Right $ " ++ showR actual

checkEithers checkLs checkRs showL showR msgHeader (Right expected) (Left actual) =
  assertFailure $ 
  msgHeader ++ ": expected is Right $ " ++ showR expected ++
  "\n" ++
  "actual is Left $ " ++ showL actual
