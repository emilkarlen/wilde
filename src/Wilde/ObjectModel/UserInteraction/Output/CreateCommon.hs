-- | Functionallity that is common to output for create.

{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ObjectModel.UserInteraction.Output.CreateCommon
       (
         AttributeTypeInfo(..),
         ATTRIBUTE_OUTPUT_FOR_CREATE(..),

         at2ati,

         UserInteractionOutputerForAnyObjectForCreate,
         AttributeWidgetDefaultValueForCreate(..),
         AttributeTypeOutputerForCreate,

         mkAttributeTypeSetup,
         mkAttributeTypeInfoForOutput,

         mkResolverConstructor_std,

         -- * Other

         mkFormBlockRowInfoForWidget,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.GenericStringRep as Gsr
import qualified Wilde.Media.UserInteraction.Output as UiO

import Wilde.ObjectModel.ObjectModel

import Wilde.ObjectModel.Presentation (ATTRIBUTE_PRESENTATION(..))
import Wilde.ObjectModel.UserInteraction
import qualified Wilde.ObjectModel.UserInteraction.Common as UiCommon
import qualified Wilde.ObjectModel.UserInteraction.Output.Common as OutputCommon
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr
import           Wilde.WildeUi.UiPrimitives


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Information about an 'AttributeType' that makes it possible to construct
-- an UI outputer for it.
data AttributeTypeInfo typeForExisting typeForCreate =
  AttributeTypeInfo
  {
    atiCrossRefKey       :: AttributeName
  , atiTitle             :: WildeTitle
  , atiPresentationO     :: PresentationOutputer           typeForExisting
  , atiOutputerForCreate :: AttributeName ->
                            AttributeTypeOutputerForCreate typeForExisting typeForCreate
  , atiGsrInputer        :: Gsr.GenericStringRepInputer                    typeForCreate
  , atiGsrOutputer       :: Gsr.GenericStringRepOutputer                   typeForCreate
  , atiCreateOption      :: Maybe (AttributeTypeCreateOption               typeForCreate)
  }

-- | Extracts the information we need about an 'AttributeType'.
at2ati :: ATTRIBUTE_OUTPUT_FOR_CREATE atConf
       => AttributeType       atConf dbTable typeForExisting typeForCreate
       -> AttributeTypeInfo                  typeForExisting typeForCreate
at2ati at@(AttributeType {
              atCrossRefKey   = theCrossRefKey,
              atPresentationO = thePresO
              })
  =
  AttributeTypeInfo
  {
    atiCrossRefKey       = theCrossRefKey
  , atiTitle             = atTitle at
  , atiPresentationO     = thePresO
  , atiOutputerForCreate = atOutputerForCreate at
  , atiGsrInputer        = OmGsr.atInputerForCreate  at
  , atiGsrOutputer       = OmGsr.atOutputerForCreate at
  , atiCreateOption      = atCreateOption at
  }

type UserInteractionOutputerForAnyObjectForCreate typeForExisting typeForCreate =
  UiO.WidgetConstructorForObjectWithDefault
  (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)

mkFormBlockRowInfoForWidget :: AttributeName
                            -> WildeTitle
                            -> UiO.WidgetConstructorForObjectWithDefault
                               (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
                            -> Maybe (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
                            -> UiO.ObjectName
                            -> UiO.FormBlockRowInfo
mkFormBlockRowInfoForWidget attributeName title widgetConstructor defaultValue objectName =
  UiO.mkFormBlockRowInfoForLabelAndWidget (label,widget)
  where
    ek = UiO.elementKey objectName attributeName
    titleString = wildeStyled title
    widget      = widgetConstructor defaultValue objectName
    label       = UiO.Label ek titleString


-------------------------------------------------------------------------------
-- - Translation to structures in Output.Common -
-------------------------------------------------------------------------------


mkAttributeTypeSetup :: AttributeTypeInfo typeForExisting typeForCreate
                     -> OutputCommon.AttributeTypeSetup
                        (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
                        typeForCreate
mkAttributeTypeSetup ati =
  OutputCommon.AttributeTypeSetup
  {
    OutputCommon.setupInfo     = mkAttributeTypeInfoForOutput ati
  , OutputCommon.setupResolver = mkResolverConstructor_std (atiCreateOption ati)
  }

mkAttributeTypeInfoForOutput :: AttributeTypeInfo typeForExisting typeForCreate
                             -> OutputCommon.AttributeTypeInfo
                                (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
                                typeForCreate
mkAttributeTypeInfoForOutput (AttributeTypeInfo {
                                 atiCrossRefKey       = theAttributeName,
                                 atiTitle             = theTitleWithStyle,
                                 atiPresentationO     = thePresentationOutputer,
                                 atiOutputerForCreate = theAttributeTypeOutputerForCreate,
                                 atiGsrInputer        = theGsrInputer,
                                 atiGsrOutputer       = theGsrOutputer
                                })
  =
  OutputCommon.AttributeTypeInfo
  {
    OutputCommon.atiCrossRefKey    = theAttributeName
  , OutputCommon.atiTitle          = theTitleWithStyle
  , OutputCommon.atiWidgetOutputer = theAttributeTypeOutputerForCreate
  , OutputCommon.atiGsrOutputer    = theGsrOutputer
  }

mkResolverConstructor_std
  :: forall typeForExisting typeForCreate.
     Maybe (AttributeTypeCreateOption typeForCreate)
  -> OutputCommon.AttributeFixAndDefaultResolver
     (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
     typeForCreate
mkResolverConstructor_std mbCreateOption = do
  inputMediaLookuper <- UiCommon.getLookuperOfInputValueForRoleFromEnv
  pure $ retVal inputMediaLookuper
  where
    retVal :: (UiCommon.Role -> AttributeName -> UiO.ObjectName -> Maybe String)
           -> AttributeName
           -> UiO.ObjectName
           -> OutputCommon.FixAndDefault (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
              typeForCreate
    retVal inputMediaLookuper attributeName objectName =
      OutputCommon.FixAndDefault
      {
        OutputCommon.appFad = theAppResolver
      , OutputCommon.envFad = theEnvResolver
      }

      where
        theAppResolver :: OutputCommon.FixAndDefaultForApplicationConfiguration (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
        theAppResolver =
          OutputCommon.FixAndDefaultForApplicationConfiguration
          {
            OutputCommon.appValsFix     = theAppFix
          , OutputCommon.appValsDefault = theAppDefault
          }

        theEnvResolver :: OutputCommon.FixAndDefaultForEnvironment (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate) typeForCreate
        theEnvResolver =
          OutputCommon.FixAndDefaultForEnvironment
          {
            OutputCommon.envValsFix     = Left <$> inputSpecialValueFromEnv UiCommon.Fix
          , OutputCommon.envValsDefault = DefaultCreateFromUiPreFill <$> inputSpecialValueFromEnv UiCommon.Default
          }

        theAppFix = case mbCreateOption of
          Just (AtuicoFixed c) -> Just OutputCommon.FixFromApplicationHasPrecedence
          _                    -> Nothing

        theAppDefault = case mbCreateOption of
          Just (AtuicoDefault (Left  s)) -> Just $ DefaultCreateFromUiPreFill s
          Just (AtuicoDefault (Right c)) -> Just $ DefaultCreateFromCreate c
          _                              -> Nothing

        inputSpecialValueFromEnv :: UiCommon.Role -> Maybe String
        inputSpecialValueFromEnv role =
          inputMediaLookuper role attributeName objectName
