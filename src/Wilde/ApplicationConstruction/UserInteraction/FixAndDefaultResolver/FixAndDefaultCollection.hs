-------------------------------------------------------------------------------
-- | Resolvment of Fix and Default values from a collection of values for an
-- object.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.FixAndDefaultResolver.FixAndDefaultCollection
       (
         -- * Construction of resolver and setup 
         
         mkAttributeTypeSetup,
         mkResolverConstructor,

         -- * Collection Fixed and Default values
         
         -- ** Default values
         
         AttributeWidgetDefaultValue(..),
         AttributeWidgetDefaultValues,
                  
         -- ** Fixed and default values
         
         FixedOrDefaultedValue(..),
         FixedAndDefaultValues(..),
         lookupFixedOrDefaultValue,
         
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List
import Data.Monoid hiding (Any)

import qualified Wilde.Media.GenericStringRep as Gsr
import qualified Wilde.Media.UserInteraction.Output as UiO

import Wilde.ObjectModel.ObjectModel

import Wilde.ObjectModel.UserInteraction
import qualified Wilde.ObjectModel.UserInteraction.Output.Common as OutputCommon

import qualified Wilde.ObjectModel.UserInteraction.Output.CreateCommon as CreateCommon


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------

  
mkAttributeTypeSetup :: ATTRIBUTE_OUTPUT_FOR_CREATE atConf
                     => FixedAndDefaultValues atConf dbTable
                     -> AttributeType atConf dbTable typeForExisting typeForCreate
                     -> OutputCommon.AttributeTypeSetup
                        (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
                        typeForCreate
mkAttributeTypeSetup fixedAndDefaults at =
  OutputCommon.AttributeTypeSetup
  {
    OutputCommon.setupInfo                = CreateCommon.mkAttributeTypeInfoForOutput .
                                            CreateCommon.at2ati $
                                            at
  , OutputCommon.setupResolverConstructor = mkResolverConstructor fixedAndDefaults at
  }

-- | A resolver that resolves no values from the \"appliation\", but
-- values from the environment according to the values given as argument
-- to the function.
mkResolverConstructor :: FixedAndDefaultValues atConf dbTable
                      -> AttributeType atConf dbTable typeForExisting typeForCreate
                      -> AttributeName
                      -> UiO.ObjectName
                      -> OutputCommon.FixAndDefaultResolver
                         (AttributeWidgetDefaultValueForCreate typeForExisting typeForCreate)
                         typeForCreate
mkResolverConstructor fixAndDefaults at
  attributeName objectName
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
        OutputCommon.appFix     = Nothing
      , OutputCommon.appDefault = return Nothing
      }

    theEnvResolver =
      OutputCommon.FixAndDefaultResolverForEnvironment
      {
        OutputCommon.envFix = case fixOrDefault of
           Just (FixedOrDefaultedIsFixed gsr) -> return $ Just (Left gsr)
           _ -> return Nothing

      , OutputCommon.envDefault = case fixOrDefault of
           Just (FixedOrDefaultedIsDefaulted genericWidgetDefault) ->
             return (Just (DefaultCreateFromUiPreFill genericWidgetDefault))
           _ -> return Nothing
      }

    fixOrDefault = lookupFixedOrDefaultValue fixAndDefaults attributeName


-------------------------------------------------------------------------------
-- - Fixed and Default values -
-------------------------------------------------------------------------------


-- | Fixed and defaulted values for zero or more attributes for an object type.
data FixedAndDefaultValues atConf dbTable =
  FixedAndDefaultValues
  {
    fadvFixed     :: AttributeFixedValues         atConf dbTable
  , fadvDefaulted :: AttributeWidgetDefaultValues atConf dbTable
  }

instance Monoid (FixedAndDefaultValues atConf dbTable) where 
  mempty = FixedAndDefaultValues [] []
  mappend (FixedAndDefaultValues f1 d1) (FixedAndDefaultValues f2 d2) =
    FixedAndDefaultValues (f1 ++ f2) (d1 ++ d2)


-------------------------------------------------------------------------------
-- | Looks up a fixed or defaulted value.
--
-- Fixed values have precedence over defaulted values.
-------------------------------------------------------------------------------
lookupFixedOrDefaultValue :: FixedAndDefaultValues atConf dbTable
                          -> AttributeName
                          -> Maybe FixedOrDefaultedValue
lookupFixedOrDefaultValue fixedsAndDefaults atTargetKey =
  case lookupFixedValue (fadvFixed fixedsAndDefaults ) atTargetKey of
    Just x -> Just (FixedOrDefaultedIsFixed x)
    Nothing ->
      case lookupWidgetDefaultValue (fadvDefaulted fixedsAndDefaults) atTargetKey of
        Just x  -> Just (FixedOrDefaultedIsDefaulted x)
        Nothing -> Nothing

-- | Information about either a fixed or defaulted value.
data FixedOrDefaultedValue = FixedOrDefaultedIsFixed     Gsr.GenericStringRep
                           | FixedOrDefaultedIsDefaulted UiO.GenericWidgetDefaultValue




-------------------------------------------------------------------------------
-- - Widget Default Values -
-------------------------------------------------------------------------------


-- | Default values for some or all of the 'AttributeType's of an 'ObjectType'.
type AttributeWidgetDefaultValues atConf dbTable = 
  [AttributeWidgetDefaultValue atConf dbTable]

-- | A default value for the widget of an 'AttributeType'.
data AttributeWidgetDefaultValue atConf dbTable =
  AttributeWidgetDefaultValue
  {
    widgetDefaultType  :: Any (AttributeType atConf dbTable)
  , widgetDefaultValue :: UiO.GenericWidgetDefaultValue
    -- ^ Information about a default value that the \"widget constructor\"
    -- should use to initialize the widget.
    --
    -- The format depends on the widget.
    --
    -- It could be the \"Generic String Representation\", or any other
    -- string representation.
    --
    -- If the Widget Constructor does not understand the value, it is
    -- allowed to ignore it.  The value is only used for convenience.
    --
    -- /Example/: for an attribute that is a number and who's widget
    -- is a string that represents an expression: the default value
    -- may be an expression.  It can be an invalid expression, e.g.
    -- \"10*NUM_UNITS - 0.1*NUM_DISCOUNTS\",
    -- where \"NUM_UNITS\" and \"NUM_DISCOUNTS\" are reminders for the
    -- user about what information to supply.
  }

-- | Looks up a default value for a given 'AttributeType'.
lookupWidgetDefaultValue :: AttributeWidgetDefaultValues atConf dbTable
                         -> AttributeName
                         -> Maybe UiO.GenericWidgetDefaultValue
lookupWidgetDefaultValue defaults atTargetKey =
  maybe
  Nothing 
  (Just . widgetDefaultValue)
  (find isMatch defaults)
  where
    isMatch :: AttributeWidgetDefaultValue atConf dbTable -> Bool
    isMatch (AttributeWidgetDefaultValue at _) =
      atTargetKey == atCrossRefKey_anyValue at
