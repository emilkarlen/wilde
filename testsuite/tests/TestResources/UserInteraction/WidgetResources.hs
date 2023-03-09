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

import           Wilde.Render.Html.Types
import qualified Wilde.Render.Html.Attribute as HA
import qualified Wilde.Render.Html.Element as HE
import qualified Text.Blaze.Html.Renderer.String as HRS

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
    html_expected = HRS.renderHtml $ UiO.widgetHtml expected
    html_actual   = HRS.renderHtml $ UiO.widgetHtml actual


-------------------------------------------------------------------------------
-- - Construction of test-widget -
-------------------------------------------------------------------------------


mkWidgetConstructorGetterForAt :: (defaultType -> String)
                               -> UiO.AttributeName
                               -> UiO.WidgetConstructorGetter defaultType
mkWidgetConstructorGetterForAt showDefaultValue atName =
  pure $ mkWidget showDefaultValue atName

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
  widgetHtml (StringWidgetForTest s) = HE.htmlString s
