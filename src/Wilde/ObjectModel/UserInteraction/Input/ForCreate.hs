{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

-- | Construction of inputers for User Interaction.
module Wilde.ObjectModel.UserInteraction.Input.ForCreate
       (
         ObjectInputResult,
         inputer,
         inputer_plain,
         inputerAny,

         -- * Information about attribute types

         AttributeTypeInfo(..),
         ATTRIBUTE_INPUT_FOR_CREATE(..),
         at2ati,
         InfoForInputAndConstructAttribute(..),

         -- * Translators

         at2InfoForInputAndConstructAttribute,

         -- * Low level helpers

         inputAttr,
         inputAttrAny,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Either

import qualified Data.List.NonEmpty as NonEmpty

import qualified Wilde.Media.ElementSet as ES

import Wilde.Database.Sql (SQL_IDENTIFIER)
import Wilde.Media.UserInteraction.Io
import qualified Wilde.Media.UserInteraction.Input as UiI
import qualified Wilde.ObjectModel.UserInteraction.Input.Common as UiICommon

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.ObjectModelUtils as OmUtils
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr
import           Wilde.ObjectModel.UserInteraction as OmUi


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Types -
-------------------------------------------------------------------------------


-- | Information about an 'AttributeType' that makes it possible to
-- construct an UI outputer for it.
data AttributeTypeInfo dbTable typeForExisting typeForCreate =
  (Show typeForExisting,
   Typeable typeForExisting,
   Wilde.Database.Sql.SQL_IDENTIFIER dbTable)
  =>
  AttributeTypeInfo
  {
    atiCrossRefKey        :: AttributeName
  , atiDbTable            :: dbTable -- Just needed for typing
  , atiCreateOption       :: Maybe (AttributeTypeCreateOption typeForCreate)
  , atiInputer            :: AttributeName -> UiI.UserInteractionInputer (ES.ElementInputResult typeForCreate)
  , atiGenericStringRepI  :: OmGsr.GenericStringRepInputer typeForCreate
  }

-- | Extracts the information we need about an 'AttributeType'.
at2ati :: ATTRIBUTE_INPUT_FOR_CREATE atConf
       => AttributeType atConf dbTable typeForExisting typeForCreate
       -> AttributeTypeInfo    dbTable typeForExisting typeForCreate
at2ati at@(AttributeType {
              atCrossRefKey = theCrossRefKey
              })
  =
  AttributeTypeInfo
  {
    atiCrossRefKey        = theCrossRefKey
  , atiDbTable            = undefined
  , atiCreateOption       = atCreateOption at
  , atiInputer            = atInputerForCreate at
  , atiGenericStringRepI  = OmGsr.atInputerForCreate at
  }

-- | A 'AttributeTypeInfo' together with an 'AttributeType'-annotation value.
data InfoForInputAndConstructAttribute atConf dbTable typeForExisting typeForCreate =
  InfoForInputAndConstructAttribute
  {
    atiaaTypeInfo   :: AttributeTypeInfo dbTable typeForExisting typeForCreate
  , atiaaAnnotation :: atConf dbTable typeForExisting typeForCreate
  , atiaaForAttributeConstruction :: AttributeType atConf dbTable typeForExisting typeForCreate
  }

-- | Constructs the necessary information about a single 'AttributeType'.
--
-- Use this function temporary instead of 'getAtInfo' while an 'Attribute' need to
-- have 'AttributeType'.
at2InfoForInputAndConstructAttribute :: ATTRIBUTE_INPUT_FOR_CREATE atConf
                                     => AttributeType atConf dbTable typeForExisting typeForCreate
                                     -- ^ TODO Replace with only annotation type when all info is
                                     -- in the annotation.
                                     -> InfoForInputAndConstructAttribute
                                        atConf dbTable typeForExisting typeForCreate
at2InfoForInputAndConstructAttribute at =
  InfoForInputAndConstructAttribute
  {
    atiaaTypeInfo   = at2ati at
  , atiaaAnnotation = atConfiguration at
  , atiaaForAttributeConstruction = at
  }


-------------------------------------------------------------------------------
-- - Inputer constructors -
-------------------------------------------------------------------------------


inputer :: ATTRIBUTE_INPUT_FOR_CREATE atConf
        => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
        -- ^ Type of 'Object' in the form.
        -> ObjectName
        -- ^ The object's name in the input form.
        -> UiI.Monad (ObjectInputResult
                      (ObjectForCreate otConf atConf dbTable otNative idAtExisting idAtCreate))
inputer ot objectName = inputerNoClass at2InfoForInputAndConstructAttribute ot objectName

inputer_plain :: ATTRIBUTE_INPUT_FOR_CREATE atConf
        => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
        -- ^ Type of 'Object' in the form.
        -> ObjectName
        -- ^ The object's name in the input form.
        -> UiI.Monad (ObjectForCreate otConf atConf dbTable otNative idAtExisting idAtCreate)
inputer_plain ot objectName =
  do
    errOrRes <- inputer ot objectName
    either UiI.throwErr pure errOrRes

inputerNoClass :: (forall e c . AttributeType atConf dbTable e c
                   -> InfoForInputAndConstructAttribute atConf dbTable e c)
               -> ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
               -- ^ Type of 'Object' in the form.
               -> ObjectName
               -- ^ The object's name in the input form.
               -> UiI.Monad (ObjectInputResult
                             (ObjectForCreate otConf atConf dbTable otNative idAtExisting idAtCreate))
inputerNoClass at2InfoForInputAndConstructAttribute ot@(ObjectType {}) objectName =
  do
    idAttrR      <- inputAttr objectName iacaIdAt
    nonIdAttrRs  <- mapM (inputAttrAny objectName) iacaNonIdAts
    let allAttrRs = fmap Any idAttrR : nonIdAttrRs
    let (errors,values) = partitionEithers allAttrRs
    case errors of
      [] ->
        let
          Right idAttr = idAttrR
          nonIdAttrs   = tail values
        in
         pure $ pure $
         ObjectForCreate
         {
           ofcType            = ot
         , ofcIdAttribute     = idAttr
         , ofcNonIdAttributes = nonIdAttrs
         }
      (e:es) -> pure $ Left $ OmUi.otUiObjectInputErrorInfo
                (OmUtils.otCrossRefKey ot)
                objectName
                ((NonEmpty.:|) e es)
  where
    iacaIdAt     = at2InfoForInputAndConstructAttribute $ otIdAttributeType ot
    iacaNonIdAts = map
                   (OmUtils.anyValueApply2 at2InfoForInputAndConstructAttribute)
                   (otNonIdAttributeTypes ot)

-- | A variant of 'inputer' that acts on any type of 'ObjectType'.
inputerAny :: ATTRIBUTE_INPUT_FOR_CREATE atConf
           => (ObjectName,AnyO (ObjectType otConf atConf))
           -> UiI.Monad (ObjectInputResult (AnyO (ObjectForCreate otConf atConf)))
inputerAny = OmUtils.toAnyForOtAndArg inputer


-------------------------------------------------------------------------------
-- - Helpers -
-------------------------------------------------------------------------------


-- | Inputer of a single 'AttributeForCreate' of an 'ObjectForCreate'.
inputAttr :: ObjectName
             -- ^ The object's name in the input form.
          -> InfoForInputAndConstructAttribute atConf dbTable typeForExisting typeForCreate
          -> UiI.Monad
             (ES.ElementInputResult (AttributeForCreate atConf dbTable typeForExisting typeForCreate))
inputAttr objectName
  (InfoForInputAndConstructAttribute
   {
     atiaaTypeInfo =
        AttributeTypeInfo
        {
          atiCreateOption = Just (AtuicoFixed value)
        }
   , atiaaForAttributeConstruction = theAttributeType
   })
  =
    pure $ pure $
    AttributeForCreate
    {
      attrfcType  = theAttributeType
    , attrfcValue = value
    }
inputAttr objectName
  (InfoForInputAndConstructAttribute
   {
     atiaaTypeInfo =
        AttributeTypeInfo
        {
          atiCrossRefKey       = theAttributeName
        , atiInputer           = theInputer
        , atiGenericStringRepI = theGsrInputer
        }
   , atiaaAnnotation = theAnnotation
   , atiaaForAttributeConstruction = theAttributeType
   })
  =
  do
    valueR <- UiICommon.inputer_fixedFromEnvHasPrecedence
              theGsrInputer theInputer
              theAttributeName objectName
    case valueR of
      Left err -> pure $ Left err
      Right value ->
        pure $ pure $
        AttributeForCreate
        {
          attrfcType  = theAttributeType
        , attrfcValue = value
        }

-- | A variant of 'inputAttr' that acts on values encapsulated in 'Any'.
inputAttrAny :: ObjectName
             -> Any (InfoForInputAndConstructAttribute atConf dbTable)
             -> UiI.Monad
                (ES.ElementInputResult (Any (AttributeForCreate atConf dbTable)))
inputAttrAny objectName (Any x) =
  do
    attrRes <- inputAttr objectName x
    pure $ fmap Any attrRes

lookupFixedAttributeGenericStringRep :: ElementKey
                                     -> ES.Lookuper (ES.ElementInputResult
                                                     (ElementKey,
                                                      Maybe OmGsr.GenericStringRep))
lookupFixedAttributeGenericStringRep ek set =
  pure $ either Left (\mbValue -> Right (ek,mbValue)) mbValueR
  where
    mbValueR = ES.lookupSingleton_optional ek set
