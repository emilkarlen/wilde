-- | Utilities related to testing.
module TestResources.Testing.TestUtils
       (
         identityTests,
       )
       where

import Test.HUnit


-------------------------------------------------------------------------------
-- | Helper method for constructing tests of identity functions.
-------------------------------------------------------------------------------
identityTests :: (Eq a,Show a)
              => (a -> a)     -- ^ The function to test.
                              -- Expected behaviour is identity.
              -> String       -- ^ Name of this "group" of tests.
              -> [(String,a)] -- ^ Each test in the group (description,value).
              -> Test
identityTests identityFunction groupName tests = TestList $ map oneTest tests
  where
    oneTest (testName,val) = (groupName ++ " (" ++ testName ++ ")") ~:
                              val ~=? (identityFunction val)
