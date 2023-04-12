{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ObjectModel.UserInteraction.Output.Common
(
  AttributeTypeInfo(..),

  AttributeOutputValueSpecification(..),

  -- * Construction of a FormBlock

  AttributeTypeSetup(..),
  attributeOutputer,
  outputerForSetupConstructor,
  objectOutputer,

  module Wilde.ObjectModel.UserInteraction.Output.FixAndDefault,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.AnyValue2 as AnyValue2

import           Wilde.WildeUi.UiPrimitives

import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.GenericStringRep as Gsr

import           Wilde.ObjectModel.ObjectModel

import qualified Wilde.ObjectModel.UserInteraction.Common as UiCommon
import           Wilde.ObjectModel.UserInteraction.Output.FixAndDefault


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Information about an 'AttributeType' that makes it possible to construct
-- an UI outputer for it.
--
-- The outputer supports a default value of a parametrized type
-- @defaultValue@ and possibly an existing value of type @attrValue@.
-------------------------------------------------------------------------------
data AttributeTypeInfo defaultValue attrValue =
  AttributeTypeInfo
  {
    atiCrossRefKey    :: AttributeName
  , atiTitle          :: WildeTitle
  , atiWidgetOutputer :: AttributeName -> UiO.WidgetConstructorGetter defaultValue
  , atiGsrOutputer    :: Gsr.GenericStringRepOutputer attrValue
  }


-------------------------------------------------------------------------------
-- | The information about an \"Attribute Type\" necessary to produce
-- User Interaction Output for a single attribute.
-------------------------------------------------------------------------------
data AttributeTypeSetup d a =
  AttributeTypeSetup
  {
    setupInfo     :: AttributeTypeInfo d a
  , setupResolver :: AttributeFixAndDefaultResolver d a
  }


-------------------------------------------------------------------------------
-- - handling of fix and default values -
-------------------------------------------------------------------------------


-- | Specifies what is output for an attribute in an input form.
data AttributeOutputValueSpecification d a
  = NoOutput
    -- ^ The attribute is abscent from the form.
  | FixValue (Either Gsr.GenericStringRep a)
    -- ^ The attribute has a fixed value so the form should contain no widget
    -- for inputing a value.
    --
    -- The fix value may be displayed in the form or not.
  | WidgetWithPossibleDefaultValue (Maybe d)
    -- ^ Either 'Just' a default value, or no default value.
    --
    -- The form should contain an input widget for the attribute.
    -- If there is Just a value, then the widget should have this
    -- value preselected.
    -- Otherwise the widget decides itself what value should be
    -- preselected.


-------------------------------------------------------------------------------
-- | Resolves the \"Attribute Output Value Specification\" using the
-- standard preferences, given resovlers for fix and default values.
-------------------------------------------------------------------------------
resolveAttributeOutputValueSpecification
  :: forall d a.
     (UiO.ObjectName -> FixAndDefault d a)
  -> UiO.ObjectName
  -> AttributeOutputValueSpecification d a
resolveAttributeOutputValueSpecification on2Fad objectName =
  resolve (on2Fad objectName)
  where
    resolve :: FixAndDefault d a -> AttributeOutputValueSpecification d a
    resolve
      (FixAndDefault
       appConf@(FixAndDefaultForApplicationConfiguration appFix appDefault)
       envConf@(FixAndDefaultForEnvironment envFix envDefault))
      =
        maybe
          outputIsEitherDefaultValueOrNoValue
          outputIsFixValue
          mbFixValue
      where
        mbFixValue = resolveFixValue appFix envFix

        outputIsFixValue Nothing  = NoOutput
        outputIsFixValue (Just x) = FixValue x

        outputIsEitherDefaultValueOrNoValue =
            let
              mbDefaultValue = resolveWidgetDefaultValue appDefault envDefault
            in
              WidgetWithPossibleDefaultValue mbDefaultValue


-------------------------------------------------------------------------------
-- - Construction of UI ouput for an Attribute Type -
-------------------------------------------------------------------------------

attributeOutputer :: forall d a.
                     AttributeTypeSetup d a
                  -> UiO.Monad (UiO.ObjectName -> UiO.FormBlockRowInfo)
attributeOutputer (AttributeTypeSetup atInfo getFadCon) =
  do
    fadCon            <- getFadCon
    widgetConstructor <- atiWidgetOutputer atInfo attributeName
    let
      on2fad  :: UiO.ObjectName -> FixAndDefault d a
      on2fad   = fadCon attributeName

      on2ovs  :: UiO.ObjectName -> AttributeOutputValueSpecification d a
      on2ovs   = resolveAttributeOutputValueSpecification on2fad

    pure $ retVal on2ovs widgetConstructor
  where
    attributeName :: AttributeName
    attributeName  = atiCrossRefKey atInfo

    theGsrOutputer = atiGsrOutputer atInfo

    retVal :: (UiO.ObjectName -> AttributeOutputValueSpecification d a)
           -> UiO.WidgetConstructorForObjectWithDefault d
           -> UiO.ObjectName
           -> UiO.FormBlockRowInfo
    retVal on2ovs widgetCon objectName = retVal' (on2ovs objectName)
      where
        retVal' :: AttributeOutputValueSpecification d a
                -> UiO.FormBlockRowInfo
        retVal' NoOutput = empty

        retVal' (FixValue fixValue) =
          UiO.mkFormBlockRowInfoForMetas $
          UiCommon.metaValuesForRole UiCommon.Fix attributeName objectName $
          either id theGsrOutputer fixValue

        retVal' (WidgetWithPossibleDefaultValue mbDefaultValue) =
          UiO.mkFormBlockRowInfoForLabelAndWidget (label,widget)
          where
            widget      = widgetCon mbDefaultValue objectName
            label       = UiO.Label ek title
            title       = wildeStyled $ atiTitle atInfo
            ek          = UiO.elementKey objectName attributeName


-------------------------------------------------------------------------------
-- | Constructs an outputer for 'AttributeType's for a single 'ObjectType'.
-------------------------------------------------------------------------------
outputerForSetupConstructor :: (Any (AttributeType atConf dbTable)
                                -> AnyValue2.Container AttributeTypeSetup)
                            -- ^ Constructs setup for an 'AttributeType'.
                            -> [Any (AttributeType atConf dbTable)]
                            -- ^ The attributes that should be input via the form,
                            -- and the order they should be displayed in it.
                            -> UiO.Monad (UiO.ObjectName -> UiO.FormBlock)
outputerForSetupConstructor setupConstructor attributeTypesOrder =
  objectOutputer atSetups_any
  where
    atSetups_any = map setupConstructor attributeTypesOrder

-------------------------------------------------------------------------------
-- | Outputs a list of attributes as part of an object.
-------------------------------------------------------------------------------
objectOutputer :: [AnyValue2.Container AttributeTypeSetup]
               -> UiO.Monad (UiO.ObjectName -> UiO.FormBlock)
objectOutputer setups =
  do
    attrOutputers <- mapM mkAttrOutputter setups
    pure $ retVal attrOutputers
  where
    retVal :: [UiO.ObjectName -> UiO.FormBlockRowInfo]
           -> UiO.ObjectName -> UiO.FormBlock
    retVal attrOutputers objectName =
      UiO.concatAtFormBlockInfos
        [attrOutputer objectName | attrOutputer <- attrOutputers]

    mkAttrOutputter :: AnyValue2.Container AttributeTypeSetup
                    -> UiO.Monad (UiO.ObjectName -> UiO.FormBlockRowInfo)
    mkAttrOutputter (AnyValue2.Container atSetup) = attributeOutputer atSetup
