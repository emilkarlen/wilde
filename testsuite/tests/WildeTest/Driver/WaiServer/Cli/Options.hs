module WildeTest.Driver.WaiServer.Cli.Options
(
    theTest
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified Wilde.Driver.Application.WaiServer.Cli.Options as Sut

import TestResources.Testing.AssertUtils (isLeft)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest =
    TestList
    [
        "false" ~:
        TestList
        [
          valid_false ~: checkEq valid_false (Right False)
          | valid_false <- Sut.values_false
        ]
    ,   "true" ~:
        TestList
        [
          valid_true ~: checkEq valid_true (Right True)
          | valid_true <- Sut.values_true
        ]
    ,   "invalid" ~:
        TestList
        [
          invalid ~: check invalid isLeft
          | invalid <- ["not_a_bool", "2", "-1"]
        ]
    ]

check :: String -> (Either String Bool -> Assertion) -> Test
check input expectation = TestCase $
    expectation actual
    where
        actual = Sut.read_bool input

checkEq :: String -> Either String Bool -> Test
checkEq input expected = check input (assertEqual "" expected)
