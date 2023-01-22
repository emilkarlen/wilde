module WildeTest.ApplicationConstruction.AttributeTypeTest
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Map as Map

import Data.Time.Calendar

import Test.HUnit

import qualified TestResources.TestData as TD

import qualified Wilde.Media.ElementSet as ES

import Wilde.Database.Sql

import Wilde.Media.UserInteraction.Io
import qualified Wilde.Media.UserInteraction.Input as UiI
import Wilde.ObjectModel.ObjectModel
import qualified Wilde.ApplicationConstruction.ObjectModel.ObjectType as OT

import qualified Wilde.ObjectModel.UserInteraction.Input.ForCreate as InputForCreate


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | All tests in this module.
theTest :: Test
theTest = TestLabel "AttributeTypeTest" $
          TestList
          [
            test_word32
          , test_double
          , test_date
          , test_string
          , test_primaryKeyTypeReference
          ]

-------------------------------------------------------------------------------
-- - Result checker for input of an attribute from UI -
-------------------------------------------------------------------------------


type AttributeUiInputResultChecker a = ResultChecker (ES.ElementInputResult a)


-------------------------------------------------------------------------------
-- - Word32 -
-------------------------------------------------------------------------------


test_word32 :: Test
test_word32 =
  TestLabel "UI InputWord32" $
  standardTestsOfMandatoryAndOptional
  checkMandatory checkOptional
  okInputs invalidSyntaxInputs
  valueMissingForMandatory_NothingForOptional
  where
    checkMandatory = testInputSingleValueAttribute $ OT.at_Word32          TD.inputWidth Field OT.noDefault "title"
    checkOptional  = testInputSingleValueAttribute $ OT.at_Word32_optional TD.inputWidth Field OT.noDefault "title"

    okInputs :: [([UiI.ElementValue],OT.Word32)]
    okInputs = singletonInputs
      [
        ("1"    ,1)
      , ("001"  ,1)
      , ("  1"  ,1)
      , ("0"    ,0)
      , ("0000" ,0)
        -- operators
      , ("+1"   , 1)
      , ("1+2"  , 3)
      , ("2-1"  , 1)
      , ("2*3"  , 6)
      , ("5/2"  , 2)
      , (" 1  + 2  " ,3)  -- spaces
      , ("1+2*3"   ,7)    -- precedence
      , ("(1+2)*3" ,9)    -- parenthesis
      ]
    invalidSyntaxInputs :: [[UiI.ElementValue]]
    invalidSyntaxInputs =
      [
        ["0:8"]
      , ["a"]
      , ["1","1"]
      , ["1","2"]
      , ["",""]
      ]


-------------------------------------------------------------------------------
-- - Double -
-------------------------------------------------------------------------------


test_double :: Test
test_double =
  TestLabel "UI Input Double" $
  standardTestsOfMandatoryAndOptional
  checkMandatory checkOptional
  okInputs invalidSyntaxInputs
  valueMissingForMandatory_NothingForOptional
  where
    checkMandatory = testInputSingleValueAttribute $ OT.at_Double_withExprUiInput          TD.inputWidth Field OT.noDefault "title"
    checkOptional  = testInputSingleValueAttribute $ OT.at_Double_optional_withExprUiInput TD.inputWidth Field OT.noDefault "title"

    okInputs :: [([UiI.ElementValue],Double)]
    okInputs = singletonInputs
      [
        ("1"    ,1)
      , ("001"  ,1)
      , ("  1"  ,1)
      , ("1.0"  ,1)
      , ("0"    ,0)
      , ("0000" ,0)
        -- operators
      , ("-1"   ,(-1))
      , ("+1"   , 1)
      , ("1+2"  , 3)
      , ("2-1"  , 1)
      , ("2*3"  , 6)
      , ("5/2"  , 2.5)
      , (" 1  + 2  " ,3)  -- spaces
      , ("1+2*3"   ,7)    -- precedence
      , ("(1+2)*3" ,9)    -- parenthesis
      , ("1.23"    ,1.23) -- decimals
      ]
    invalidSyntaxInputs :: [[UiI.ElementValue]]
    invalidSyntaxInputs =
      [
        ["0:8"]
      , ["a"]
      , ["8a"]
      , ["1","1"]
      , ["1","2"]
      , ["",""]
      ]


-------------------------------------------------------------------------------
-- - String -
-------------------------------------------------------------------------------


test_string :: Test
test_string =
  TestLabel "UI Input String" $
  standardTestsOfMandatoryAndOptional
  checkMandatory checkOptional
  okInputs invalidSyntaxInputs
  valueMissingForMandatory_NothingForOptional
  where
    checkMandatory = testInputSingleValueAttribute $ OT.at_String          TD.noDbIo 100 TD.inputWidth Field OT.noDefault "title"
    checkOptional  = testInputSingleValueAttribute $ OT.at_String_optional TD.noDbIo 100 TD.inputWidth Field OT.noDefault "title"

    okInputs :: [([UiI.ElementValue],String)]
    okInputs = singletonInputs
      [
        ("s" ,"s")
      , (""  ,"")
      ]
    invalidSyntaxInputs :: [[UiI.ElementValue]]
    invalidSyntaxInputs =
      [
        ["1","1"]
      , ["",""]
      ]

    valueMissingForMandatory_NothingForOptional :: [[UiI.ElementValue]]
    valueMissingForMandatory_NothingForOptional =
      [
        []
      ]


-------------------------------------------------------------------------------
-- - Date -
-------------------------------------------------------------------------------


test_date :: Test
test_date =
  TestLabel "UI Input Date" $
  standardTestsOfMandatoryAndOptional
  checkMandatory checkOptional
  okInputs invalidSyntaxInputs
  valueMissingForMandatory_NothingForOptional
  where
    checkMandatory = testInputSingleValueAttribute $ OT.at_Date          TD.inputWidth Field OT.noDefault "title"
    checkOptional  = testInputSingleValueAttribute $ OT.at_Date_optional TD.inputWidth Field OT.noDefault "title"

    okInputs :: [([UiI.ElementValue],Day)]
    okInputs =
      [
        ( ["2012-12-12"],(fromGregorian 2012 12 12))
      ]
    invalidSyntaxInputs :: [[UiI.ElementValue]]
    invalidSyntaxInputs =
      [
        ["1-10-10"]
      , ["10-10-10"]
      , ["110-10-10"]
      , ["   10-10-10"]
      , ["a"]
      , ["   a"]
      , ["2012-12-12","2012-12-12"]
      , ["2012-12-12","2013-13-13"]
      , ["",""]
      ]


-------------------------------------------------------------------------------
-- - PrimaryKey -
-------------------------------------------------------------------------------


test_primaryKeyTypeReference :: Test
test_primaryKeyTypeReference =
  TestLabel "UI Input Reference to PrimaryKeyType" $
  standardTestsOfMandatoryAndOptional
  checkMandatory checkOptional
  okInputs invalidSyntaxInputs
  valueMissingForMandatory_NothingForOptional
  where
    checkMandatory = testInputSingleValueAttribute $ OT.at_ref_std          Field TD.otsPkName TD.rpsPkName Nothing
    checkOptional  = testInputSingleValueAttribute $ OT.at_ref_std_optional Field TD.otsPkName TD.rpsPkName Nothing

    okInputs :: [([UiI.ElementValue],OT.PrimaryKeyType)]
    okInputs = singletonInputs
      [
        ("1"    ,1)
      , ("001"  ,1)
      , ("  1"  ,1)
      , ("0"    ,0)
      , ("0000" ,0)
      , ("123967540" ,123967540)
        -- operators
      , ("+1"   , 1)
      , ("1+2"  , 3)
      , ("2-1"  , 1)
      , ("2*3"  , 6)
      ]
    invalidSyntaxInputs :: [[UiI.ElementValue]]
    invalidSyntaxInputs =
      [
        ["0:8"]
      , ["a"]
      , ["1","1"]
      , ["1","2"]
      , ["",""]
      ]


-------------------------------------------------------------------------------
-- - ResultCheckers -
-------------------------------------------------------------------------------


type UserInput = [String]

-- | Checks the result of an 'UiI.Monad'.
type ResultChecker a = UserInput -> UiI.Result a -> Assertion


-- | Checks for success and an exact value.
exactly :: (Eq a,Show a)
        => a
        -> ResultChecker a
exactly expected uiInput (Right actual) = assertEqual "msg-prefix" expected actual
exactly expected uiInput (Left  err   ) = assertFailure $ show uiInput ++ ": Expected success. Got " ++ show err

-- | Checks for success, regardles of actual value.
anySuccess :: (Eq a,Show a)
           => ResultChecker a
anySuccess ui (Right _)   = return ()
anySuccess ui (Left  err) = assertFailure $ show ui ++ ": Expected success (any value). Got " ++ show err

-- | Checks for error, regardles of actual kind.
anyError :: (Eq a,Show a)
         => ResultChecker a
anyError ui (Right x)   = assertFailure $ show ui ++ ": Expected error. Got success with " ++ show x
anyError ui (Left  err) = return ()

-- | Checks for errors that match a given predicate.
thisError :: (Eq a,Show a)
          => ES.ElementLookupErrorType -- ^ Expected error type
          -> AttributeUiInputResultChecker a
thisError expectedType uInput (Right (Left err@(_,actualType,_))) =
  assertBool errMsg (actualType == expectedType)
  where
    errMsg = show uInput ++ ": Expected error of type " ++ show expectedType ++
             ". Got error of type " ++ show actualType
thisError expectedType uInput x  =
  assertFailure $ show uInput ++ ": Expected ElementLookupError failure of type " ++
  show expectedType ++
  ". Got " ++ show x

-- | Checks for an error that says that a value is missing.
valueMissingError :: (Eq a,Show a) => AttributeUiInputResultChecker a
valueMissingError = thisError ES.ValueMissing

-- | Checks for an error that says that a value has invalid syntax.
invalidSyntaxError :: (Eq a,Show a) => AttributeUiInputResultChecker a
invalidSyntaxError = thisError ES.InvalidSyntax


-------------------------------------------------------------------------------
-- - Standard Tests Constructors -
-------------------------------------------------------------------------------


-- | Standad tests of a mandatory value.
--
-- This method makes it easy to reuse test input data.
--
-- The method is a result of the observation that tests of a mandatory
-- and optional attribute differs only in the one aspect:
-- input that gives 'valueMissingError' for a mandatory, gives
-- 'Nothing' for an optional.
standardTestsOfMandatoryAndOptional
  :: (Eq a,Show a)
  => (AttributeUiInputResultChecker a
      -> [UiI.ElementValue]
      -> Test)
  -> (AttributeUiInputResultChecker (Maybe a)
      -> [UiI.ElementValue]
      -> Test)       -- ^ Test construct for a given (checker,input)
  -> [([UiI.ElementValue],a)] -- ^ Each gives: success
  -> [[UiI.ElementValue]]     -- ^ Each gives: 'invalidSyntaxError'
  -> [[UiI.ElementValue]]     -- ^ 'valueMissingError' for mandatory, 'Nothing' for optional
  -> Test
standardTestsOfMandatoryAndOptional checkMandatory checkOptional
  okInputs
  invalidSyntaxInputs
  valueMissingForMandatory_NothingForOptional =
  TestList
  [
    TestLabel "mandatory" $
    standardTestsOfMandatory
    checkMandatory
    okInputs invalidSyntaxInputs valueMissingForMandatory_NothingForOptional
    ,
    TestLabel "optional" $
    standardTestsOfOptional
    checkOptional
    okInputs invalidSyntaxInputs valueMissingForMandatory_NothingForOptional
   ]

-- | Standad tests of a mandatory value.
standardTestsOfMandatory :: (Eq a,Show a)
                         => (AttributeUiInputResultChecker a
                             -> [UiI.ElementValue]
                             -> Test)       -- ^ Test construct for a given (checker,input)
                         -> [([UiI.ElementValue],a)] -- ^ Each gives: success
                         -> [[UiI.ElementValue]]     -- ^ Each gives: 'invalidSyntaxError'
                         -> [[UiI.ElementValue]]     -- ^ Each gives: 'valueMissingError'
                         -> Test
standardTestsOfMandatory check okInputs invalidSyntaxInputs valueMissingInputs =
  let
    check_ok (input,expected) = check (exactly (Right expected)) input
  in
   TestList
   [
     valueTests check_ok                   okInputs
   , tests      (check invalidSyntaxError) invalidSyntaxInputs
   , tests      (check valueMissingError)  valueMissingInputs
   ]

-- | Standad tests of an optional value.
standardTestsOfOptional :: (Eq a,Show a)
                        => (AttributeUiInputResultChecker (Maybe a)
                            -> [UiI.ElementValue]
                            -> Test)       -- ^ Test construct for a given (checker,input)
                        -> [([UiI.ElementValue],a)] -- ^ Each gives: success 'Just' a
                        -> [[UiI.ElementValue]]     -- ^ Each gives: 'invalidSyntaxError'
                        -> [[UiI.ElementValue]]     -- ^ Each gives: success 'Nothing'
                        -> Test
standardTestsOfOptional check okJustInputs invalidSyntaxInputs okNothingInputs =
  let
    check_just (input,expected) = check (exactly (Right (Just expected))) input
  in
   TestList
   [
     valueTests check_just                        okJustInputs
   , tests      (check invalidSyntaxError)        invalidSyntaxInputs
   , tests      (check (exactly (Right Nothing))) okNothingInputs
   ]


-------------------------------------------------------------------------------
-- - Test constructors -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the result of trying to input a value from the User Interaction,
-- for an 'Attribute' that inputs from a single 'Element' who's
-- name is the 'ObjectName' followed by the 'AttributeType's cross-ref-key.
-------------------------------------------------------------------------------
testInputSingleValueAttribute :: (InputForCreate.ATTRIBUTE_INPUT_FOR_CREATE atConf)
                              => AttributeType atConf dbTable b a
                              -> AttributeUiInputResultChecker a -- ^ Checks the actual result
                              -> [UiI.ElementValue]              -- ^ User input. If empty, then
                                                                 -- the input map is empty.
                              -> Test
testInputSingleValueAttribute at@(AttributeType {}) checkResult userInput =
  TestCase $
  let
    attrName                     = atCrossRefKey at
    inputerOfAttrForCreateForObj = InputForCreate.atiInputer . InputForCreate.at2ati $ at
    inputerOfAttrForCreate       = inputerOfAttrForCreateForObj attrName objName
    attrElementKey               = elementKey objName attrName
    attrElementKeyStr            = elementKeyRender attrElementKey
    media                        = if null userInput
                                   then Map.empty
                                   else Map.singleton attrElementKeyStr userInput :: ES.ElementSet
    custEnv                      = ES.empty
  in
   do
     result <- UiI.run (UiI.Environment media custEnv) inputerOfAttrForCreate
     checkResult userInput result

-- | Shortcut for constructing a list of tests with a common expected result.
tests :: ([UiI.ElementValue] -> Test) -- ^ Executes a single test.
         -> [[UiI.ElementValue]]      -- ^ Each element is the input for a single test.
         -> Test
tests uiInputList = TestList . map uiInputList

-- | Shortcut for constructing a list of tests with a common expected result.
valueTests :: (Eq a,Show a)
           => (([UiI.ElementValue],a) -> Test) -- ^ Executes a single test.
           -> [([UiI.ElementValue],a)]         -- ^ List of (expected value,input)
              -> Test
valueTests expectedAndUiInputList = TestList . map expectedAndUiInputList


-------------------------------------------------------------------------------
-- - Test data -
-------------------------------------------------------------------------------


-- An arbitary object name to use by tests.
objName :: ObjectName
objName = objectName "o"

data Table = Field
             deriving Show

instance SQL_IDENTIFIER Table where
  sqlIdentifier = show


-------------------------------------------------------------------------------
-- - User Interaction Input Values Test Data -
-------------------------------------------------------------------------------


-- | Transforms test data for a singlet input string to the
-- format expected by the test methods.
singletonInputs :: [(String,a)] -> [([String],a)]
singletonInputs = map $ \(input,val) -> ([input],val)

nothingForOptional        :: [[UiI.ElementValue]]
nothingForOptional = valueMissingForMandatory_NothingForOptional

valueMissingForMandatory  :: [[UiI.ElementValue]]
valueMissingForMandatory = valueMissingForMandatory_NothingForOptional

-- | Inputs that each should give a 'valueMissingError'
-- for a mandatory value, and success of 'Nothing' for
-- optional values.
valueMissingForMandatory_NothingForOptional :: [[UiI.ElementValue]]
valueMissingForMandatory_NothingForOptional =
  [
    []
  , [""]
  , ["  "]
  ]
