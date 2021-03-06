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

-- | Functionallity that is common to output for create.
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


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Information about an 'AttributeType' that makes it possible to construct
-- an UI outputer for it.
data AttributeTypeInfo typeForExisting typeForCreate =
  AttributeTypeInfo
  {
    atiCrossRefKey       :: AttributeName
  , atiTitle             :: StyledTitle
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
                            -> StyledTitle
                            -> (UiO.WidgetConstructorForObjectWithDefault
                                (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate))
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
    OutputCommon.setupInfo                = mkAttributeTypeInfoForOutput ati
  , OutputCommon.setupResolverConstructor = mkResolverConstructor_std (atiCreateOption ati)
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

mkResolverConstructor_std :: Maybe (AttributeTypeCreateOption typeForCreate)
                          -> OutputCommon.AttributeFixAndDefaultResolverConstructor
                             (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
                             typeForCreate
mkResolverConstructor_std mbCreateOption attributeName objectName
  =
  OutputCommon.FixAndDefaultResolver
  {
    OutputCommon.appResolver = theAppResolver
  , OutputCommon.envResolver = theEnvResolver
  }
  where
    theAppResolver =
      OutputCommon.FixAndDefaultResolverForApplicationConfiguration
      {
        OutputCommon.appFix     = theAppFix
      , OutputCommon.appDefault = return theAppDefault
      }

    theEnvResolver =
      OutputCommon.FixAndDefaultResolverForEnvironment
      {
        OutputCommon.envFix     = do mbValue <- inputSpecialValueFromEnv UiCommon.Fix
                                     return $ fmap Left mbValue
      , OutputCommon.envDefault = do mbValue <- inputSpecialValueFromEnv UiCommon.Default
                                     return $ fmap DefaultCreateFromUiPreFill mbValue
      }

    theAppFix = case mbCreateOption of
      Just (AtuicoFixed c) -> Just OutputCommon.FixFromApplicationHasPrecedence
      _                    -> Nothing

    theAppDefault = case mbCreateOption of
      Just (AtuicoDefault (Left  s)) -> Just $ DefaultCreateFromUiPreFill s
      Just (AtuicoDefault (Right c)) -> Just $ DefaultCreateFromCreate c
      _                              -> Nothing

    inputSpecialValueFromEnv :: UiCommon.Role
                             -> UiO.UserInteractionOutputMonad (Maybe String)
    inputSpecialValueFromEnv role =
      do
           UiCommon.inputValueForRoleFromEnv role
                    attributeName objectName
               