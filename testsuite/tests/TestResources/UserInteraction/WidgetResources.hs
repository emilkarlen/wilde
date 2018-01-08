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

module TestResources.UserInteraction.WidgetResources
       (
         -- * Widget designed for test

         mkWidgetConstructorGetterForAt,
         mkWidget,
         anyStringWidgetForTest,
         StringWidgetForTest(..),

         -- * Checking widgets
         
         checkWidgetByHtmlString,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified Text.Html as Html

import qualified Wilde.Media.WildeMedia as WildeMedia

import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.ElementSet as ES


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


checkWidgetByHtmlString :: String -> UiO.AnyWIDGET -> UiO.AnyWIDGET -> Assertion
checkWidgetByHtmlString msgHeader (UiO.AnyWIDGET expected) (UiO.AnyWIDGET actual) = 
  assertEqual "widget html" html_expected html_actual
  where
    html_expected = show $ UiO.widgetHtml expected
    html_actual   = show $ UiO.widgetHtml actual
  

-------------------------------------------------------------------------------
-- - Construction of test-widget -
-------------------------------------------------------------------------------


mkWidgetConstructorGetterForAt :: (defaultType -> String)
                               -> UiO.AttributeName
                               -> UiO.WidgetConstructorGetter defaultType
mkWidgetConstructorGetterForAt showDefaultValue atName =
  return $ mkWidget showDefaultValue atName

-------------------------------------------------------------------------------
-- | Constructs a widget which HTML rendering is a string suited for testing.
--
--
-- Widget HTML syntax:
--
-- @
-- WidgetHtmlString ::= ElementKeyString DefaultString?
--
-- ElementKeyString ::= <normal rendering of element keys>
--
-- DefaultString    ::= <null>
--                    | FIELD_SEPARATOR "default=" <string-rep of default-value>
--
-- FIELD_SEPARATOR  ::= ":"
-- @
-------------------------------------------------------------------------------
mkWidget :: (defaultType -> String)
         -> UiO.AttributeName
         -> UiO.WidgetConstructorForObjectWithDefault defaultType
mkWidget showDefaultValue attributeName mbDefault objectName =
  anyStringWidgetForTest $
  elementKeyString ++ defaultString
  where
    elementKeyString = ES.elementKeyRender elementKey
    elementKey       = UiO.attributeElementKey objectName attributeName
    defaultString    = maybe "" (\s -> ":default=" ++ showDefaultValue s) mbDefault


-------------------------------------------------------------------------------
-- - Widget for testing -
-------------------------------------------------------------------------------

anyStringWidgetForTest :: String -> UiO.AnyWIDGET
anyStringWidgetForTest s = UiO.AnyWIDGET $ StringWidgetForTest s

newtype StringWidgetForTest = StringWidgetForTest String

-- | Widget that is a plain string.
instance UiO.WIDGET StringWidgetForTest where
  widgetHtml (StringWidgetForTest s) = Html.primHtml s
