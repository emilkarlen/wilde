-------------------------------------------------------------------------------
-- | User Interaction Input/Output for common types.
--
-- Also utilities for creating Inputers and Outputers.
-------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Wilde.ApplicationConstruction.UserInteraction.Io
       (
         AttributeTypeUiIoForExisting(..),
         AttributeTypeUserInteractionIo(..),
         AttributeTypeUiIoForCreate(..),

         uiIo_Int32,
         uiIo_Word32,
         uiIo_Word32_optional,
         uiIo_Double,
         uiIo_Double_optional,
         uiIo_Integer,
         uiIo_Integer_optional,
         uiIo_Date,
         uiIo_Date_optional,
         uiIo_Date_withConvenienteUiInput,
         uiIo_Date_optional_withConvenienteUiInput,
         uiIo_String,
         uiIo_String_optional,
         uiIo_Text,
         uiIo_Text_optional,

         uiIo_convertibleFromInteger,
         uiIo_convertibleFromInteger_optional,

         uiIo_mandatory,
         uiIo_optional,

         uiIo_asStringInM,
         uiIo_asString_optionalOnCreate,
         uiIo_asString_optional,
         uiIo_asString,
         uiIo_asTextArea,
         uiIo_asDropDown,
         uiIo_asDropDown_optional,

         -- * Utilities

         readConvertibleFromInteger_expr,

         uiIo_optional_from_mandatory,
         uiIo_forStringConvertible,

         parseEnum_optional,
         renderAttributeUiDefaultForCreate,
         renderAttributeUiDefaultForCreate_optionalOnCreate,
         renderAttributeUiDefaultForCreate_optional,
         renderAttributeUiDefaultForCreate_generic,
         renderMaybeValue,
         readUiiMonad,
         attrInput,
         attrInput_optional,
         attrInputInM,
         readUnambigousValue,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Convertible.Base

import qualified Data.Char as Char

import           Data.Word
import           Data.Int

import           Data.Time

import           Control.Monad.IO.Class

import           Wilde.Utils.Utils

import           Wilde.GenericUi.Value

import           Wilde.Media.ElementSet as ES
import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.UserInteraction.Input as UiI
import           Wilde.Media.UserInteraction.Io

import           Wilde.ObjectModel.UserInteraction.Output.CreateCommon
import qualified Wilde.ObjectModel.UserInteraction.Output.ExistingCommon as UiOExisting

import qualified Wilde.ApplicationConstruction.ElementSetUtils as ESU
import           Wilde.ApplicationConstruction.UserInteraction.Input.UserInteractionInputers
import qualified Wilde.ApplicationConstruction.UserInteraction.Input.DateParser as DateParser
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.LabelAndWidget as LabelAndWidget
import qualified Wilde.ApplicationConstruction.UserInteraction.Input.SyntaxCheck as SyntaxCheck


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type AttributeTypeUiIoForExisting a =
  UserInteractionIo
  (UiOExisting.AttributeUiDefaultForExisting a)
  (ElementInputResult a)

type AttributeTypeUiIoForCreate typeForExisting typeForCreate =
  UserInteractionIo
  (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
  (ElementInputResult typeForCreate)


-------------------------------------------------------------------------------
-- - AttributeTypeUserInteractionIo -
-------------------------------------------------------------------------------


-- | User Interaction Input and Output functionality of an 'Attribute' and
-- 'AttributeType'.
--
-- By "User Interaction" is menant interaction via an User Interface.
--
-- Parametrized by
-- a : The type of value that the 'Attribute' (and 'AttributeType') represents.
--
data AttributeTypeUserInteractionIo typeForExisting typeForCreate =
  AttributeTypeUserInteractionIo
  {
    -- | Constructs "components" for letting the user inputting
    -- an attribute as part of an object to create.
    --
    -- (The object is created by inserting it into the database).
    atuiioCreateIo :: AttributeTypeUiIoForCreate typeForExisting typeForCreate,

    -- | Constructs "components" for letting the user inputting
    -- an attribute as part of an existing object (existing = an object
    -- that does not need to be created via the database).
    -- (typeForExisting is an existing value)
    atuiioExistingIo :: AttributeTypeUiIoForExisting typeForExisting
  }


-------------------------------------------------------------------------------
-- - IO -
-------------------------------------------------------------------------------


uiIo_Int32  :: Int -> AttributeTypeUserInteractionIo Int32 Int32
uiIo_Int32 = uiIo_convertibleFromInteger

uiIo_Word32  :: Int -> AttributeTypeUserInteractionIo Word32 Word32
uiIo_Word32 = uiIo_convertibleFromInteger

uiIo_Word32_optional :: Int
                     -> AttributeTypeUserInteractionIo (Maybe Word32) (Maybe Word32)
uiIo_Word32_optional = uiIo_convertibleFromInteger_optional

uiIo_Double  :: Int
             -> AttributeTypeUserInteractionIo Double Double
uiIo_Double inputWidth = uiIo_asString inputWidth True show readDouble_expr

uiIo_Double_optional :: Int
                     -> AttributeTypeUserInteractionIo (Maybe Double) (Maybe Double)
uiIo_Double_optional inputWidth = uiIo_asString_optional inputWidth True show readDouble_expr

-- | Translates from 'Integer' to a smaller 'Integral'
--
-- Values that are out of range will be detected, iff 'safeConvert' reports an error.
uiIo_convertibleFromInteger :: (Show a,Integral a,Convertible Integer a)
                            => Int
                            -> AttributeTypeUserInteractionIo a a
uiIo_convertibleFromInteger inputWidth = uiIo_asString inputWidth True show readConvertibleFromInteger_expr

-- | Translates from 'Integer' to a smaller 'Integral'
--
-- Values that are out of range will be detected, iff 'safeConvert' reports an error.
uiIo_convertibleFromInteger_optional :: (Show a,Integral a,Convertible Integer a)
                                     => Int
                                     -> AttributeTypeUserInteractionIo (Maybe a) (Maybe a)
uiIo_convertibleFromInteger_optional inputWidth = uiIo_asString_optional inputWidth True show readConvertibleFromInteger_expr

-- | The \"target\" type can be smaller than 'Integer'.
-- This function will detect this iff 'safeConvert' detectes it, and report error.
readConvertibleFromInteger_expr :: (Show a,Integral a,Convertible Integer a)
                                => ElementKey
                                -> String
                                -> ElementInputResult a
readConvertibleFromInteger_expr elementKey input =
  do
    x <- readInteger_expr elementKey input
    case safeConvert x of
      Left _  -> Left (elementKey,
                       InvalidValue,
                       Just $ "Out of range: " ++ show x)
      Right y -> Right y

uiIo_Integer :: Int -- ^ Input Width
             -> AttributeTypeUserInteractionIo Integer Integer
uiIo_Integer inputWidth = uiIo_asString inputWidth True show readInteger_expr

uiIo_Integer_optional :: Int
                      -> AttributeTypeUserInteractionIo (Maybe Integer) (Maybe Integer)
uiIo_Integer_optional inputWidth = uiIo_asString_optional inputWidth True show readInteger_expr

uiIo_Date  :: Int -- ^ Input width
           -> AttributeTypeUserInteractionIo Day Day
uiIo_Date inputWidth = uiIo_asString inputWidth True show readUiiMonad_Date

readUiiMonad_Date :: ElementValueParser Day
readUiiMonad_Date ek singletonValue =
  if SyntaxCheck.checkDate singletonValue
    then readUiiMonad ek singletonValue
    else Left (ek,InvalidSyntax,Just singletonValue)

uiIo_Date_optional :: Int
                   -> AttributeTypeUserInteractionIo (Maybe Day) (Maybe Day)
uiIo_Date_optional inputWidth = uiIo_asString_optional inputWidth True show readUiiMonad_Date

-- | Parses a date using 'DateParser.parseEmlFormat', with
-- current date and the default locale ('defaultTimeLocale').
uiIo_Date_withConvenienteUiInput :: Int
                                 -> AttributeTypeUserInteractionIo Day Day
uiIo_Date_withConvenienteUiInput inputWidth =
  uiIo_asStringInM inputWidth True (pure show) getParser
  where
    getParser = do
      tm <- liftIO getCurrentTime
      pure $ parse (utctDay tm)

    parse :: Day -> ElementValueParser Day
    parse baseDate ek s = case DateParser.parseEmlFormat defaultTimeLocale Nothing baseDate s of
      Left _  -> Left (ek,InvalidSyntax,Just s)
      Right x -> pure x

uiIo_Date_optional_withConvenienteUiInput :: Int
                                          -> AttributeTypeUserInteractionIo (Maybe Day) (Maybe Day)
uiIo_Date_optional_withConvenienteUiInput inputWidth =
  uiIo_optional_from_mandatory $ uiIo_Date_withConvenienteUiInput inputWidth

uiIo_String  :: Int -> AttributeTypeUserInteractionIo String String
uiIo_String inputWidth = uiIo_asString inputWidth False id (\ek s -> pure s)

uiIo_String_optional :: Int
                     -> AttributeTypeUserInteractionIo (Maybe String) (Maybe String)
uiIo_String_optional inputWidth =
  uiIo_asString_optional inputWidth False id (\ek s -> pure s)

uiIo_Text :: (Int,Int)
          -> AttributeTypeUserInteractionIo String String
uiIo_Text size =
  uiIo_asTextArea size False id (\ek s -> pure s)

uiIo_Text_optional :: (Int,Int)
                   -> AttributeTypeUserInteractionIo (Maybe String) (Maybe String)
uiIo_Text_optional size =
  uiIo_optional_from_mandatory $ uiIo_asTextArea size False id (\ek s -> pure s)

uiIo_mandatory  :: (Show a,Read a)
                => Int
                -> AttributeTypeUserInteractionIo a a
uiIo_mandatory inputWidth = uiIo_asString inputWidth True show readUiiMonad

uiIo_optional :: (Show a,Read a)
              => Int
              -> AttributeTypeUserInteractionIo (Maybe a) (Maybe a)
uiIo_optional inputWidth = uiIo_asString_optional inputWidth True show readUiiMonad


-------------------------------------------------------------------------------
-- - Attribute IO -
-------------------------------------------------------------------------------


uiIo_asStringInM :: forall a.
                    Int  -- ^ Input field width
                 -> Bool -- ^ Trim input, and treat an empty string as if a value is missing.
                 -> UiO.Monad (a -> String)
                 -> UiI.Monad (ElementValueParser a)
                 -> AttributeTypeUserInteractionIo a a
uiIo_asStringInM inputWidth trimAndEmptyIsMissing getRenderValue getParseString =
   AttributeTypeUserInteractionIo
   {
     atuiioCreateIo =
      UserInteractionIo
      {
        uiOutputer = \attributeName -> do
           renderValue <- getRenderValue
           pure $ output (renderAttributeUiDefaultForCreate renderValue) attributeName
      , uiInputer  = inputter
      },
     atuiioExistingIo =
      UserInteractionIo
      {
        uiOutputer = \attributeName -> do
           renderValue <- getRenderValue
           pure $ output renderValue attributeName
      , uiInputer  = inputter
      }
   }
   where
    inputter :: AttributeName -> UiI.UserInteractionInputer (ElementInputResult a)
    inputter attributeName = attrInputInM trimAndEmptyIsMissing getParseString attributeName

    output :: (dflt -> String) -> AttributeName -> UiO.WidgetConstructorForObjectWithDefault dflt
    output renderValue attributeName mbValue objectName = LabelAndWidget.attrOutput_string inputWidth renderValue attributeName mbValue objectName

uiIo_asString_optionalOnCreate :: Int   -- ^ Input field width.
                               -> Bool  -- ^ Existing: Trim input, and treat an empty string as value-is-missing.
                               -> Bool  -- ^ Create: Trim input, and treat an empty string as a 'Nothing'.
                               -> (a -> String)
                               -> ElementValueParser a
                               -> AttributeTypeUserInteractionIo a (Maybe a)
uiIo_asString_optionalOnCreate inputWidth existingTrimAndEmptyIsMissing createTrimAndEmptyNothing
  renderValue parseString =
   AttributeTypeUserInteractionIo
   {
     atuiioCreateIo =
      UserInteractionIo
      {
        uiOutputer = \attributeName -> pure $
                     output
                     (renderAttributeUiDefaultForCreate_optionalOnCreate renderValue)
                     attributeName
      , uiInputer  = \attributeName -> attrInput_optional createTrimAndEmptyNothing parseString attributeName
      },
     atuiioExistingIo =
      UserInteractionIo
      {
        uiOutputer = \attributeName -> pure $ output renderValue attributeName
      , uiInputer  = \attributeName -> attrInput existingTrimAndEmptyIsMissing parseString attributeName
      }
   }
   where
    output :: (dflt -> String) -> AttributeName -> UiO.WidgetConstructorForObjectWithDefault dflt
    output renderValue attributeName mbValue objectName =
      LabelAndWidget.attrOutput_string inputWidth renderValue attributeName mbValue objectName

uiIo_asString_optional :: forall a. Int   -- ^ Input field width.
                       -> Bool  -- ^ Trim input, and treat an empty string as a 'Nothing'.
                       -> (a -> String)
                       -> ElementValueParser a
                       -> AttributeTypeUserInteractionIo (Maybe a) (Maybe a)
uiIo_asString_optional inputWidth trimAndEmptyNothing renderValue parseString =
   AttributeTypeUserInteractionIo
   {
     atuiioCreateIo =
      UserInteractionIo
      {
        uiOutputer = \attributeName ->  pure $
                     output
                     (renderAttributeUiDefaultForCreate_optional renderValue)
                     attributeName
      , uiInputer  = \attributeName -> attrInput_optional trimAndEmptyNothing parseString attributeName
      },
     atuiioExistingIo =
      UserInteractionIo
      {
        uiOutputer = \attributeName -> pure $ outputMaybe attributeName
      , uiInputer  = \attributeName -> attrInput_optional trimAndEmptyNothing parseString attributeName
      }
   }
  where
    output :: (dflt -> String) -> AttributeName -> UiO.WidgetConstructorForObjectWithDefault dflt
    output      renderValue attributeName mbValue objectName = LabelAndWidget.attrOutput_string inputWidth renderValue                    attributeName mbValue objectName

    outputMaybe :: AttributeName -> UiO.WidgetConstructorForObjectWithDefault (Maybe a)
    outputMaybe             attributeName mbValue objectName = LabelAndWidget.attrOutput_string inputWidth (renderMaybeValue renderValue) attributeName mbValue objectName

uiIo_optional_from_mandatory :: AttributeTypeUserInteractionIo a a
                             -> AttributeTypeUserInteractionIo (Maybe a) (Maybe a)
uiIo_optional_from_mandatory
  atUiIo@(AttributeTypeUserInteractionIo
  {
    atuiioCreateIo    = UserInteractionIo mandatoryOutpCreate   mandatoryInpCreate
  , atuiioExistingIo  = UserInteractionIo mandatoryOutpExisting mandatoryInpExisting
  })
  =
  AttributeTypeUserInteractionIo
  {
    atuiioCreateIo =
      UserInteractionIo
      {
        uiOutputer = outputerCreate
      , uiInputer  = \attributeName -> mkOptionalInp (mandatoryInpCreate attributeName)
      },
    atuiioExistingIo =
      UserInteractionIo
      {
        uiOutputer = \attributeName -> mkOptionalOutp (mandatoryOutpExisting attributeName)
      , uiInputer  = \attributeName -> mkOptionalInp  (mandatoryInpExisting attributeName)
      }
  }
  where
    outputerCreate = \attributeName ->
      do
        mandatoryOutputer <- mandatoryOutpCreate attributeName
        pure $ \mbDefault objectName ->
          case mbDefault of
            Nothing -> mandatoryOutputer Nothing objectName
            Just d  -> mandatoryOutputer (fromOptionalDefault d) objectName

    mkOptionalInp :: (ObjectName -> UiI.Monad (ElementInputResult a))
                  -> ObjectName  -> UiI.Monad (ElementInputResult (Maybe a))
    mkOptionalInp mandatoryInputer objectName =
      inputer_optional_from_mandatory $ mandatoryInputer objectName

    mkOptionalOutp :: UiO.Monad (UiO.WidgetConstructorForObjectWithDefault a)
                   -> UiO.Monad (UiO.WidgetConstructorForObjectWithDefault (Maybe a))
    mkOptionalOutp m =
      do
        mandatory <- m
        pure $ \mbMbA objectName ->
          case mbMbA of
            Nothing   -> mandatory Nothing objectName
            Just mbX  -> mandatory mbX objectName

    fromOptionalDefault :: AttributeWidgetDefaultValueForCreate (Maybe a) (Maybe a)
                        -> Maybe (AttributeWidgetDefaultValueForCreate a a)
    fromOptionalDefault (DefaultCreateFromUiPreFill x) = Just (DefaultCreateFromUiPreFill x)
    fromOptionalDefault (DefaultCreateFromExisting  x) = DefaultCreateFromExisting <$> x
    fromOptionalDefault (DefaultCreateFromCreate    x) = DefaultCreateFromCreate   <$> x

uiIo_asString :: Int  -- ^ Input width
              -> Bool -- ^ Trim input, and treat an empty string as if a value is missing.
              -> (a -> String)
              -> ElementValueParser a
              -> AttributeTypeUserInteractionIo a a
uiIo_asString inputWidth = uiIo_forStringConvertible (LabelAndWidget.attrOutput_string inputWidth)

uiIo_asTextArea :: (Int,Int)
                   -- ^ text-area (width,height)
                -> Bool
                   -- ^ Trim input, and treat an empty string as if a value is missing.
                -> (a -> String)
                -> ElementValueParser a
                -> AttributeTypeUserInteractionIo a a
uiIo_asTextArea size = uiIo_forStringConvertible (LabelAndWidget.attrOutput_textBox size)


uiIo_asDropDown :: (Eq a,Show a,Read a)
                => [(a,AnyVALUE)] -- ^ values
                -> AttributeTypeUserInteractionIo a a
uiIo_asDropDown values =
  uiIo_forStringConvertible
  attrOutput True
  renderValue parseValue
  where
    attrOutput  = LabelAndWidget.attrOutput_dropDown False options
    options     = [(show key,pres) | (key,pres) <- values]
    renderValue = show
    parseValue  = parseEnum values

uiIo_asDropDown_optional :: (Eq a,Show a,Read a)
                         => [(a,AnyVALUE)] -- ^ values
                         -> AttributeTypeUserInteractionIo (Maybe a) (Maybe a)
uiIo_asDropDown_optional values =
  uiIo_forStringConvertible_optional
  attrOutput True
  renderValue parseValue
  where
    attrOutput  = LabelAndWidget.attrOutput_dropDown True options
    options     = [(show key,pres) | (key,pres) <- values]
    renderValue = show
    parseValue  = parseEnum_optional values

parseEnum :: (Eq a,Read a)
          => [(a,AnyVALUE)]
             -- ^ values
          -> ElementValueParser a
parseEnum values ek "" = Left (ek, ES.ValueMissing, Nothing)
parseEnum values ek valueAsString =
  case readCompletelyAndUnambigously valueAsString of
        Nothing -> Left (ek,InvalidSyntax,Just valueAsString)
        Just v  -> maybe
                   (Left (ek,InvalidValue,Just valueAsString))
                   (const $ pure v)
                   (lookup v values)

parseEnum_optional :: (Eq a,Read a)
                   => [(a,AnyVALUE)]
                      -- ^ values
                   -> ElementValueParser (Maybe a)
parseEnum_optional values ek "" = Right Nothing
parseEnum_optional values ek valueAsString =
  case readCompletelyAndUnambigously valueAsString of
        Nothing -> Left (ek,InvalidSyntax,Just valueAsString)
        Just v  -> maybe
                   (Left (ek,InvalidValue,Just valueAsString))
                   (const $ pure (Just v))
                   (lookup v values)

trimAndEmptyIsMissing :: ES.Parser String String
trimAndEmptyIsMissing input =
  case dropWhile Char.isSpace input of
    [] -> Left ValueMissing
    s  -> pure s

uiIo_forStringConvertible :: (forall defaultValue . LabelAndWidget.AttrOutputAsString defaultValue)
                          -> Bool -- ^ Trim input, and treat an empty string as if a value is missing.
                          -> (a -> String)
                          -> ElementValueParser a
                          -> AttributeTypeUserInteractionIo a a
uiIo_forStringConvertible attrOutputForDefault trimAndEmptyIsMissing
  renderValue parseString =
    AttributeTypeUserInteractionIo
    {
      atuiioCreateIo =
         UserInteractionIo
         { uiOutputer = \attributeName -> pure $
                                          output
                                          (renderAttributeUiDefaultForCreate renderValue)
                                          attributeName
         , uiInputer = \attributeName -> attrInput trimAndEmptyIsMissing parseString attributeName
         },

      atuiioExistingIo =
        UserInteractionIo
        { uiOutputer = \attributeName -> pure $
                                         output
                                         renderValue
                                         attributeName
        , uiInputer  = \attributeName -> attrInput
                                         trimAndEmptyIsMissing
                                         parseString
                                         attributeName
        }
    }
    where
      output :: (defaultValue -> String)
             -> AttributeName
             -> UiO.WidgetConstructorForObjectWithDefault defaultValue
      output renderDefault attributeName mbDefault objectName =
        attrOutputForDefault renderDefault attributeName mbDefault objectName


uiIo_forStringConvertible_optional
  ::  (forall defaultValue . LabelAndWidget.AttrOutputAsString defaultValue)
  -- ^ Creates a widget with possibility to input a "missing" value
  -> Bool -- ^ Trim input, and treat an empty string as if a value is missing.
  -> (a -> String)
  -> ElementValueParser (Maybe a)
  -> AttributeTypeUserInteractionIo (Maybe a) (Maybe a)
uiIo_forStringConvertible_optional attrOutputForDefault trimAndEmptyIsMissing
  renderValue parseString =
    AttributeTypeUserInteractionIo
    {
      atuiioCreateIo =
         UserInteractionIo
         { uiOutputer = \attributeName -> pure $ output
                                          (renderAttributeUiDefaultForCreate_optional renderValue)
                                          attributeName
         , uiInputer = \attributeName -> attrInput_optional2 trimAndEmptyIsMissing parseString attributeName
         },

      atuiioExistingIo =
        UserInteractionIo
        { uiOutputer = \attributeName -> pure $ output renderValueMb attributeName
        , uiInputer  = \attributeName -> attrInput_optional2 trimAndEmptyIsMissing parseString attributeName
        }
    }
    where
      -- renderValueMb :: Maybe a -> String
      renderValueMb = maybe "" renderValue

      output :: (defaultValue -> String)
             -> AttributeName
             -> UiO.WidgetConstructorForObjectWithDefault defaultValue
      output renderDefault attributeName mbDefault objectName =
        attrOutputForDefault renderDefault attributeName mbDefault objectName


renderAttributeUiDefaultForCreate :: (a -> String)
                                  -> AttributeWidgetDefaultValueForCreate a a
                                  -> String
renderAttributeUiDefaultForCreate renderValue =
  renderAttributeUiDefaultForCreate_generic renderValue renderValue

renderAttributeUiDefaultForCreate_optionalOnCreate :: (a -> String)
                                                   -> AttributeWidgetDefaultValueForCreate a (Maybe a)
                                                   -> String
renderAttributeUiDefaultForCreate_optionalOnCreate renderValue =
  renderAttributeUiDefaultForCreate_generic renderValue (maybe "" renderValue)

renderAttributeUiDefaultForCreate_optional :: (a -> String)
                                           -> AttributeWidgetDefaultValueForCreate (Maybe a) (Maybe a)
                                           -> String
renderAttributeUiDefaultForCreate_optional renderValue =
  renderAttributeUiDefaultForCreate_generic renderOptionalValue renderOptionalValue
  where
    renderOptionalValue = maybe "" renderValue


-- | Renders an 'AttributeWidgetDefaultValueForCreate' for unrelated types for existing och for-create.
renderAttributeUiDefaultForCreate_generic :: (e -> String)
                                          -> (c -> String)
                                          -> AttributeWidgetDefaultValueForCreate e c
                                          -> String
renderAttributeUiDefaultForCreate_generic renderValueE renderValueC (DefaultCreateFromUiPreFill genericWidgetDefaultValue) = genericWidgetDefaultValue
renderAttributeUiDefaultForCreate_generic renderValueE renderValueC (DefaultCreateFromExisting x) = renderValueE x
renderAttributeUiDefaultForCreate_generic renderValueE renderValueC (DefaultCreateFromCreate   x) = renderValueC x

-- A renderer of (Maybe a) in terms of the renderer of a.
renderMaybeValue :: (a -> String) -> Maybe a -> String
renderMaybeValue renderValue Nothing  = ""
renderMaybeValue renderValue (Just v) = renderValue v











readUiiMonad :: Read a => ElementValueParser a
readUiiMonad ek singletonValue =
  maybe
  (Left (ek,InvalidSyntax,Just singletonValue))
  Right
  (readUnambigousValue singletonValue)

readUiiMonadMaybeString ek s = case s of
  "" -> pure Nothing
  _  -> pure $ Just s






-- | Inputer for a mandatory value.
attrInput :: Bool -- ^ Trim input, and treat an empty string as if a value is missing.
          -> ElementValueParser a
          -> AttributeName
          -> ObjectName
          -> UiI.Monad (ElementInputResult a)
attrInput trimAndEmptyIsMissing parseString attributeName objectName =
  UiI.inInputMedia_raw theLookuper
  where
    -- theLookuper :: ES.Lookuper a
    theLookuper set = do
      singletonValue <- ESU.lookupSingleton_maybeTrim trimAndEmptyIsMissing ek set
      parseString ek singletonValue

    ek = UiI.elementKey objectName attributeName

-- | Inputer for an optional value, in terms of a parser of a mandatory one.
attrInput_optional :: Bool -- ^ Trim input, and treat an empty string as 'Nothing'.
                   -> ElementValueParser a
                   -> AttributeName
                   -> ObjectName
                   -> UiI.Monad (ElementInputResult (Maybe a))
attrInput_optional trimAndEmptyNothing parseString attributeName objectName =
  UiI.inInputMedia_raw theLookuper
  where
    -- theLookuper :: ES.Lookuper (Maybe a)
    theLookuper set = do
      mbSingletonValue <- ESU.lookupSingleton_optional_maybeTrim trimAndEmptyNothing ek set
      maybe (pure Nothing) (fmap Just . parseString ek) mbSingletonValue

    ek = (objectName,attributeName)

-- | Inputer for an optional value, in terms of a parser of an optional one.
--
-- A missing value can be represented in two ways:
--
-- (1) the value is missing in the UI-input
-- (2) the value exists in the UI-input but is empty
attrInput_optional2 :: Bool -- ^ Trim input, and treat an empty string as 'Nothing'.
                    -> ElementValueParser (Maybe a)
                    -> AttributeName
                    -> ObjectName
                    -> UiI.Monad (ElementInputResult (Maybe a))
attrInput_optional2 trimAndEmptyNothing parseString attributeName objectName =
  UiI.inInputMedia_raw theLookuper
  where
    -- theLookuper :: ES.Lookuper (Maybe a)
    theLookuper set = do
      mbSingletonValue <- ESU.lookupSingleton_optional_maybeTrim trimAndEmptyNothing ek set
      maybe (pure Nothing) (parseString ek) mbSingletonValue

    ek = (objectName,attributeName)

-- | Inputer for a mandatory value.
attrInputInM :: Bool -- ^ Trim input, and treat an empty string as if a value is missing.
             -> UiI.Monad (ElementValueParser a)
             -> AttributeName
             -> ObjectName
             -> UiI.Monad (ElementInputResult a)
attrInputInM trimAndEmptyIsMissing getParseString attributeName objectName =
  do
    parseString <- getParseString
    attrInput trimAndEmptyIsMissing parseString attributeName objectName

readUnambigousValue :: Read a => String -> Maybe a
readUnambigousValue s =  case readsPrec 0 s of
  [(value,"")] -> Just value
  _            -> Nothing
