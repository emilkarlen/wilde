{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

-- | Construction of inputers for User Interaction.
module Wilde.ObjectModel.UserInteraction.Input.ForExisting
       (
         ObjectInputResult,
         inputer,
         inputerAny,

         -- * Information about attribute types

         AttributeInputer,
         AttributeTypeInfo(..),
         ATTRIBUTE_INPUT_FOR_EXISTING(..),
         InfoForInputAndConstructAttribute(..),

         at2ati,

         -- * Translators

         at2InfoForInputAndConstructAttribute,

         -- * Low level helpers

         inputAttr,
         inputAttrNoClass,
         inputAttrAny,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Either

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Media.WildeMedia hiding (otKey)
import Wilde.Media.ElementSet
import Wilde.Media.UserInteraction.Io
import qualified Wilde.Media.UserInteraction.Input as UiI

import qualified Wilde.ObjectModel.GenericStringRep as OmGsr
import Wilde.ObjectModel.ObjectModelUtils as OmUtils
import Wilde.ObjectModel.UserInteraction
import qualified Wilde.ObjectModel.UserInteraction.Input.Common as UiICommon


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Types -
-------------------------------------------------------------------------------


-- | Information about an 'AttributeType' that makes it possible to
-- construct an UI outputer for it.
data AttributeTypeInfo typeForExisting =
  (Show typeForExisting, Typeable typeForExisting)
  =>
  AttributeTypeInfo
  {
    atiCrossRefKey      :: AttributeName
  , atiPresentationO    :: PresentationOutputer typeForExisting
  , atiInputer          :: AttributeInputer typeForExisting
  , atiGsrInputer       :: OmGsr.GenericStringRepInputer typeForExisting
  }

-- | Extracts the information we need about an 'AttributeType'.
at2ati :: ATTRIBUTE_INPUT_FOR_EXISTING atConf
       => AttributeType atConf dbTable typeForExisting typeForCreate
       -> AttributeTypeInfo            typeForExisting
at2ati at@(AttributeType {
              atCrossRefKey   = theCrossRefKey,
              atPresentationO = thePresentationO
              })
  =
  AttributeTypeInfo
  {
    atiCrossRefKey   = theCrossRefKey
  , atiPresentationO = thePresentationO
  , atiInputer       = atInputerForExisting at
  , atiGsrInputer    = OmGsr.atInputerForExisting at
  }

-- | A 'AttributeTypeInfo' together with an 'AttributeType'-annotation value.
data InfoForInputAndConstructAttribute atConf dbTable typeForExisting typeForCreate =
  InfoForInputAndConstructAttribute
  {
    atiaaTypeInfo   :: AttributeTypeInfo typeForExisting
  , atiaaAnnotation :: atConf dbTable typeForExisting typeForCreate
  , atiaaForAttributeConstruction :: AttributeType atConf dbTable typeForExisting typeForCreate
  }

-- | Constructs the necessary information about a single 'AttributeType'.
--
-- Use this function temporary instead of 'getAtInfo' while an 'Attribute' need to
-- have 'AttributeType'.
at2InfoForInputAndConstructAttribute :: ATTRIBUTE_INPUT_FOR_EXISTING atConf
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


-- | A variant of 'inputer' that acts on any type of 'ObjectType'.
inputerAny :: ATTRIBUTE_INPUT_FOR_EXISTING atConf
           => (ObjectName,AnyO (ObjectType otConf atConf))
           -> UiI.Monad (ObjectInputResult (AnyO (Object otConf atConf)))
inputerAny = toAnyForOtAndArg inputer

inputer :: ATTRIBUTE_INPUT_FOR_EXISTING atConf
        => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
        -- ^ Type of 'Object' in the form.
        -> ObjectName
        -- ^ The object's name in the input form.
        -> UiI.Monad (ObjectInputResult
                      (Object otConf atConf dbTable otNative idAtExisting idAtCreate))
inputer = inputerNoClass at2InfoForInputAndConstructAttribute

inputerNoClass :: (forall e c . AttributeType atConf dbTable e c
                   -> InfoForInputAndConstructAttribute atConf dbTable e c)
               -> ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
               -- ^ Type of 'Object' in the form.
               -> ObjectName
               -- ^ The object's name in the input form.
               -> UiI.Monad (ObjectInputResult
                             (Object otConf atConf dbTable otNative idAtExisting idAtCreate))
inputerNoClass at2InfoForInputAndConstructAttribute ot objectName =
  do
    idAttrR      <- inputAttrNoClass objectName iacaIdAt
    nonIdAttrRs  <- sequence $ map (inputAttrAny objectName) iacaNonIdAts
    let allAttrRs = (fmap Any idAttrR) : nonIdAttrRs
    let (errors,values) = partitionEithers allAttrRs
    case errors of
      [] ->
        let
          Right idAttr = idAttrR
          nonIdAttrs   = tail values
        in
         return $ return (conObject ot idAttr nonIdAttrs)
      (e:es) -> return $ Left $ otUiObjectInputErrorInfo
                (OmUtils.otCrossRefKey ot)
                objectName
                (NonEmpty.mk e es)
  where
    iacaIdAt     = at2InfoForInputAndConstructAttribute $ otIdAttributeType ot
    iacaNonIdAts = map
                   (OmUtils.anyValueApply2 at2InfoForInputAndConstructAttribute)
                   (otNonIdAttributeTypes ot)

-------------------------------------------------------------------------------
-- | Inputer of a single 'Attribute' of an 'Object'.
-------------------------------------------------------------------------------
inputAttr :: ATTRIBUTE_INPUT_FOR_EXISTING atConf
          => ObjectName
          -- ^ The object's name in the input form.
          -> AttributeType atConf dbTable typeForExisting typeForCreate
          -> UiI.Monad (ElementInputResult
                        (Attribute atConf dbTable typeForExisting typeForCreate))
inputAttr objectName at =
  inputAttrNoClass objectName (at2InfoForInputAndConstructAttribute at)

-------------------------------------------------------------------------------
-- | Inputer of a single 'Attribute' of an 'Object'.
-------------------------------------------------------------------------------
inputAttrNoClass :: ObjectName
                 -- ^ The object's name in the input form.
                 -> InfoForInputAndConstructAttribute atConf dbTable typeForExisting typeForCreate
                 -> UiI.Monad
                    (ElementInputResult
                     (Attribute atConf dbTable typeForExisting typeForCreate))
inputAttrNoClass objectName
  info@(InfoForInputAndConstructAttribute
        {
          atiaaTypeInfo   =
             AttributeTypeInfo
             {
               atiCrossRefKey   = theAttributeName
             , atiInputer       = theWidgetInputer
             , atiPresentationO = thePresentationO
             , atiGsrInputer    = theGsrInputer

             }
        , atiaaAnnotation = theAnnotation
        , atiaaForAttributeConstruction = theAttributeType
        }) =
   do
     valueR <- UiICommon.inputer_fixedFromEnvHasPrecedence
               theGsrInputer theWidgetInputer
               theAttributeName objectName
     return $ fmap newPlainAttr valueR
  where
    newPlainAttr value =
      Attribute
      {
        attrType         = theAttributeType
      , attrValue        = value
      , attrPresentation = return presentation
      }
      where
        presentation = thePresentationO value

-------------------------------------------------------------------------------
-- | A variant of 'inputAttr' that acts on values encapsulated in 'Any'.
-------------------------------------------------------------------------------
inputAttrAny :: ObjectName
             -> Any (InfoForInputAndConstructAttribute atConf dbTable)
             -> UiI.Monad
                (ElementInputResult (Any (Attribute atConf dbTable)))
inputAttrAny objectName (Any x) =
  do
    attrRes <- inputAttrNoClass objectName x
    return $ fmap Any attrRes
