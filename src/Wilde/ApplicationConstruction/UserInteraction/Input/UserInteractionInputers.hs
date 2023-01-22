-------------------------------------------------------------------------------
-- | Utilities for constructing 'UiIo.UserInteractionInputer'.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.Input.UserInteractionInputers
       (
         mkUiInputer,
         mkUiInputer_c,

         readInteger_expr,
         readDouble_expr,

         inputer_optional_from_mandatory,
         inputerForObjectName_optional_from_mandatory,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Char

import Wilde.Media.ElementSet

import Wilde.Media.MonadWithInputMedia

import qualified Wilde.Media.ElementSet as ES

import qualified Wilde.Media.UserInteraction.Io as UiIo
import qualified Wilde.Media.UserInteraction.Input as UiI

import qualified Wilde.ApplicationConstruction.UserInteraction.Input.EvaluateExpression as Eval


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


mkUiInputer :: (ElementKey -> Lookuper a)
            -> UiIo.AttributeName
            -> UiI.UserInteractionInputer (ElementInputResult a)
mkUiInputer lookuper attributeName objectName =
  inInputMedia_raw $ lookuper ek
  where
    ek = UiIo.elementKey objectName attributeName :: ElementKey

mkUiInputer_c :: (Parser (Maybe [ElementValue]) a)
            -> UiIo.AttributeName
            -> UiI.UserInteractionInputer (ElementInputResult a)
mkUiInputer_c lookupConverter = mkUiInputer (mkLookuper lookupConverter)

readDouble_expr :: ElementKey -> String -> ElementInputResult Double
readDouble_expr elementKey input =
  case dropWhile isSpace input of
    "" -> Left (elementKey,ES.ValueMissing,Nothing)
    s  -> case Eval.evalDouble s of
            (Left err) -> Left (elementKey,ES.InvalidSyntax,Just (Eval.errorString err))
            (Right x)  -> Right x

readInteger_expr :: ElementKey -> String -> ElementInputResult Integer
readInteger_expr elementKey input =
  case dropWhile isSpace input of
    "" -> Left (elementKey,ES.ValueMissing,Nothing)
    s  -> case Eval.evalInteger s of
            (Left err) -> Left (elementKey,ES.InvalidSyntax,Just (Eval.errorString err))
            (Right x)  -> Right x

-- | Treats a missing value as 'Nothing'.
inputer_optional_from_mandatory :: UiI.Monad (ElementInputResult a)
                                -> UiI.Monad (ElementInputResult (Maybe a))
inputer_optional_from_mandatory ma =
  do
    result <- ma
    return $
         case result of
           Right a  -> Right (Just a)
           Left err@(_,errorType,_) ->
             if   errorType ==  ES.ValueMissing
             then Right Nothing
             else Left err

-- Uses the inputer for the mandatory AttributeType in the target
-- to make an inputer for an optional value.
-- What need to be done is to catch ValueMissing and make a Nothing from it.
inputerForObjectName_optional_from_mandatory :: (UiIo.AttributeName -> UiIo.ObjectName -> UiI.Monad (ElementInputResult a))
                                             -> (UiIo.AttributeName -> UiIo.ObjectName -> UiI.Monad (ElementInputResult (Maybe a)))
inputerForObjectName_optional_from_mandatory inputer_mandatory attributeName objectName =
  inputer_optional_from_mandatory (inputer_mandatory attributeName objectName)
