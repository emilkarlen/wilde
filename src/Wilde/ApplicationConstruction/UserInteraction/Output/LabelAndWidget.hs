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

module Wilde.ApplicationConstruction.UserInteraction.Output.LabelAndWidget
       (
         AttrOutputAsString,

         attrOutput_string,
         attrOutput_textBox,

         attrOutput_dropDown,

         widget_dropDownList,

         attrOutput_oneOfManyAttribute,

         nonMonadicSimpleUiOutputer,
         atUiOutputerEfromC,

         -- * Utilities
         
         newAttributeOutput,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.GenericUi.Value

import Wilde.WildeUi.StdValueTypes

import Wilde.Media.GenericStringRep
import Wilde.Media.Element

import Wilde.Media.UserInteraction

import Wilde.ObjectModel.ObjectModel

import Wilde.ApplicationConstruction.UserInteraction.Widgets
import Wilde.ObjectModel.UserInteraction.Output.CreateCommon
import qualified Wilde.ObjectModel.UserInteraction.Output.ExistingCommon as UiOExisting


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - AttrOutputAsString -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Short cut for method that construct 'LabelAndWidget' from strings,
-- given a custom type of default value.
-- (The custom type of default value is needed since it can be
-- both 'AttributeWidgetDefaultValueForCreate' (for create), and
-- 'AttributeUiDefaultForExisting' (for existing)).
-------------------------------------------------------------------------------
type AttrOutputAsString defaultValue = (defaultValue -> String)
                                       -> AttributeName
                                       -> Maybe defaultValue
                                       -> ObjectName
                                       -> AnyWIDGET


-------------------------------------------------------------------------------
-- - String and Text -
-------------------------------------------------------------------------------


-- | Attribute output as a single line string widget
attrOutput_string :: Int 
                  -> AttrOutputAsString defaultValue
attrOutput_string inputWidth renderDefault attributeName defaultValue objectName =
  newAttributeOutput elementKey widgetInfo
  where
    elementKey = attributeElementKey objectName attributeName
    widgetInfo = LineInputInfo
                {
                  lineSize    = inputWidth
                , lineDefault = fmap renderDefault defaultValue
                }

-- | Attribute output as a single line string widget
attrOutput_textBox :: (Int,Int) -- ^ (width,height)
                   -> AttrOutputAsString a
attrOutput_textBox size renderValue attributeName defaultValue objectName =
  newAttributeOutput (objectName,attributeName) widgetInfo
  where
    widgetInfo = TextAreaInfo
                {
                  textAreaSize  = size
                , textDefault   = fmap renderValue defaultValue
                }


-------------------------------------------------------------------------------
-- - Drop Down -
-------------------------------------------------------------------------------


-- | Attribute output as a drop down widget
attrOutput_dropDown :: Bool
                    -> [(GenericStringRep,AnyVALUE)]
                    -> AttrOutputAsString defaultValue
attrOutput_dropDown optional values renderDefault attributeName defaultValue objectName =
  newAttributeOutput (objectName,attributeName) widgetInfo
  where
    widgetInfo = DropDownListInfo
              {
                dropDownOptions = options
              , dropDownDefault = fmap renderDefault defaultValue
              }
    options = if optional then ("",dropDownNullValueWithSomeWidth) : values else values

widget_dropDownList :: Bool -> MultiWidgetConstructor
widget_dropDownList optional mbKey attributeName values objectName =
  AnyWIDGET $ newWidgetWithKey (objectName,attributeName) widgetInfo
  where
    widgetInfo = DropDownListInfo
                 {
                   dropDownOptions = if optional then (("",dropDownNullValueWithSomeWidth) : values) else values
                 , dropDownDefault = mbKey
                 }

-- | Presentation value for NULL values in drop down lists.
--
-- The value have some width > 2.
dropDownNullValueWithSomeWidth :: AnyVALUE
dropDownNullValueWithSomeWidth = AnyVALUE $ QuotedStringValue $ concat $ replicate 7 "&nbsp";

-------------------------------------------------------------------------------
-- | 'LabelAndWidget' for an enumerated type.
-------------------------------------------------------------------------------
attrOutput_oneOfManyAttribute :: MultiWidgetConstructor
                              -> CrossRefIdentifier
                              -> [MultiItem]
                              -> Maybe GenericStringRep
                              -> ObjectName
                              -> AnyWIDGET
attrOutput_oneOfManyAttribute widgetConstructor attributeName values mbKey objectName =
  widgetConstructor mbKey attributeName values objectName

-------------------------------------------------------------------------------
-- | A 'WidgetConstructorGetter' defined in terms of a simple outputer.
--
-- The simple outputer:
-- * does not use functionality of the 'UiIO.UserInteractionOutputMonad',
-- * uses a simpler representation of the default value for-create, that uses the
-- fact that the type-for-existing and type-for-create are identical.
-------------------------------------------------------------------------------
nonMonadicSimpleUiOutputer :: (GenericWidgetDefaultValue -> Maybe a)
                           -> (ElementKey -> Maybe a -> AnyWIDGET)
                           -> AttributeName
                           -> UiOExisting.WidgetConstructorGetter (AttributeWidgetDefaultValueForCreate a a)
nonMonadicSimpleUiOutputer parseStringDefault mkWidget attributeName =
  return $ \mbDefaultC objectName ->
    let
      mbEC   = simplifyECDefault parseStringDefault mbDefaultC
      ek     = elementKey objectName attributeName       :: ElementKey
      widget = mkWidget ek mbEC                          :: AnyWIDGET
    in
     widget
  where
    simplifyECDefault :: (GenericWidgetDefaultValue -> Maybe a)
                      -> Maybe (AttributeWidgetDefaultValueForCreate a a) -> Maybe a
    simplifyECDefault _ Nothing = Nothing
    simplifyECDefault _ (Just (DefaultCreateFromExisting x))  = Just x
    simplifyECDefault _ (Just (DefaultCreateFromCreate x))    = Just x
    simplifyECDefault parse (Just (DefaultCreateFromUiPreFill s)) = parse s

atUiOutputerEfromC :: (AttributeName -> UiOExisting.WidgetConstructorGetter (AttributeWidgetDefaultValueForCreate e c))
                   -> AttributeName -> UiOExisting.WidgetConstructorGetter (UiOExisting.AttributeUiDefaultForExisting e)
atUiOutputerEfromC atUiOutputerC attributeName =
  do
    mkAttrOutputForCreateDefault <- atUiOutputerC attributeName
    return $ \mbE objectName ->
      mkAttrOutputForCreateDefault (fmap DefaultCreateFromExisting mbE) objectName

newAttributeOutput :: WIDGET widgetInfo
                   => ElementKey
                   -> widgetInfo
                   -> AnyWIDGET
newAttributeOutput key theWidgetInfo =
  AnyWIDGET withWidgetKey
  where
   withWidgetKey = WithWidgetKey
                   {
                     widgetKey  = key
                   , widgetInfo = theWidgetInfo
                   }
