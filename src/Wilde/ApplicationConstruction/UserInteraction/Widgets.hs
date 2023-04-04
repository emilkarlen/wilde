-- | Implementation of some common widgets.
--
-- Also some tools for constructing widgets.
module Wilde.ApplicationConstruction.UserInteraction.Widgets
       (
         -- * Re-exporting

         WIDGET(..),
         AnyWIDGET(..),

         -- * Widget with key

         WithWidgetKey,

         newWidgetWithKey,
         newWidgetWithKeyAny,

         mkLabelAndWidgetFromInfo,

         -- * Some common widgets

         CheckBox,
         CheckBoxInfo(..),

         LineInput,
         LineInputInfo(..),

         TextArea,
         TextAreaInfo(..),

         DropDownList,
         DropDownListInfo(..),
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Maybe

import           Wilde.Render.Html.Types ( Html, HtmlAttr )
import qualified Wilde.Render.Html.Attribute as HA
import qualified Wilde.Render.Html.Element as HE

import           Wilde.GenericUi.Widget
import           Wilde.GenericUi.Value

import           Wilde.Media.Element
import           Wilde.Media.UserInteraction (LabelAndWidget)
import           Wilde.Media.GenericStringRep


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - WithWidgetKey -
-------------------------------------------------------------------------------


-- | A widget with a single key.
data WithWidgetKey a =
  WithWidgetKey
  {
    widgetKey  :: ElementKey
  , widgetInfo :: a
  }

instance WIDGET widgetInfo => WIDGET (WithWidgetKey widgetInfo) where
  widgetHtml withWidgetKey = htmlForWithWidgetKey withWidgetKey widgetHtml

newWidgetWithKey :: ElementKey -> widgetInfo -> WithWidgetKey widgetInfo
newWidgetWithKey key theWidgetInfo =
   WithWidgetKey
   {
     widgetKey  = key
   , widgetInfo = theWidgetInfo
   }

newWidgetWithKeyAny :: WIDGET widgetInfo
                    => ElementKey
                    -> widgetInfo
                    -> AnyWIDGET
newWidgetWithKeyAny (objectName,attributeName) theWidgetInfo =
  AnyWIDGET $ newWidgetWithKey (objectName,attributeName) theWidgetInfo

-------------------------------------------------------------------------------
-- | Constructs a 'LabelAndWidget' with the same
-- element key in the label and widget.
-------------------------------------------------------------------------------
mkLabelAndWidgetFromInfo :: WIDGET widgetInfo
                         => ElementKey
                         -> LabelString
                         -> widgetInfo
                         -> LabelAndWidget
mkLabelAndWidgetFromInfo key title theWidgetInfo =
  (label,widget)
  where
   label         = Label key title
   widget        = AnyWIDGET withWidgetKey
   withWidgetKey = WithWidgetKey
                   {
                     widgetKey  = key
                   , widgetInfo = theWidgetInfo
                   }


-------------------------------------------------------------------------------
-- - LineInput -
-------------------------------------------------------------------------------


type LineInput = WithWidgetKey LineInputInfo

data LineInputInfo =
  LineInputInfo
  {
    lineSize    :: Int
  , lineDefault :: Maybe String
  }

instance WIDGET LineInputInfo where
  widgetHtml w@(LineInputInfo
                   { lineSize    = theSize
                   , lineDefault = theDefault
                   }) =
    inputWithTypeAndAts "text" (catMaybes attributes)
     where
       thevalue   = maybe Nothing (Just . HA.value) theDefault
       thesize    = Just $ HA.size theSize :: Maybe HtmlAttr
       attributes = [thevalue,thesize] :: [Maybe HtmlAttr]


-------------------------------------------------------------------------------
-- - TextArea -
-------------------------------------------------------------------------------


type TextArea = WithWidgetKey TextAreaInfo

data TextAreaInfo =
  TextAreaInfo
  {
    textAreaSize :: (Int,Int)
  , textDefault  :: Maybe String
  }

instance WIDGET TextAreaInfo where
  widgetHtml w@(TextAreaInfo
                   { textAreaSize = (width,height)
                   , textDefault  = theDefault
                   }) =
    HE.textarea thevalue `HE.withAttrs` attributes
     where
       thevalue   = maybe HE.empty HE.str theDefault
       cols       = HA.cols width  :: HtmlAttr
       rows       = HA.rows height :: HtmlAttr
       attributes = [cols,rows]  :: [HtmlAttr]


-------------------------------------------------------------------------------
-- - CheckBox -
-------------------------------------------------------------------------------


type CheckBox  = WithWidgetKey CheckBoxInfo

data CheckBoxInfo =
  CheckBoxInfo
  {
    checkBoxValue   :: String
  , checkBoxDefault :: Bool
  }

instance WIDGET CheckBoxInfo where
  widgetHtml w@(CheckBoxInfo
                   { checkBoxValue   = theValue
                   , checkBoxDefault = theDefault
                   }) =
    HE.input `HE.withAttrs` ([atType,atValue] ++ atChecked)
     where
       atType    = HA.type_ "checkbox"
       atValue   = HA.value theValue :: HtmlAttr
       atChecked = if theDefault then [HA.checked] else [] :: [HtmlAttr]


-------------------------------------------------------------------------------
-- - DropDownList -
-------------------------------------------------------------------------------


type DropDownList  = WithWidgetKey DropDownListInfo

data DropDownListInfo =
  DropDownListInfo
  {
    dropDownOptions :: [MultiItem]
  , dropDownDefault :: Maybe GenericStringRep
  }

instance WIDGET DropDownListInfo where
  widgetHtml w@(DropDownListInfo
                   { dropDownOptions = options
                   , dropDownDefault = mbDefault
                   }) =
    HE.select $ HE.seq theOptions
     where
       mkOption   = maybe optionWithoutDefault optionWithDefault mbDefault
       theOptions = map mkOption options

       -- An option in a selection list, when there is no default specified.
       optionWithoutDefault :: (String,AnyVALUE) -> Html
       optionWithoutDefault (value,pres) = HE.option (valueHtml pres) `HE.withAttrs` [HA.value value]

       -- An option in a selection list, when there is a default specified.
       optionWithDefault :: GenericStringRep -> (String,AnyVALUE) -> Html
       optionWithDefault defaultValue (value,pres) =
         let
           withoutDefault = HE.option (valueHtml pres) `HE.withAttrs` [HA.value value]
         in
          if defaultValue == value
          then withoutDefault `HE.withAttrs` [HA.selected]
          else withoutDefault


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


htmlForWithWidgetKey :: WithWidgetKey a
                     -> (a -> Html)
                     -> Html
htmlForWithWidgetKey (WithWidgetKey ek widgetInfo) renderWidgetInfo =
  renderWidgetInfo widgetInfo `HE.withAttrs` [keyAttribute]
  where
    keyAttribute = HA.name $ elementKeyRender ek :: HtmlAttr

-- | Constructs a Html "input" tag with type and key.
inputWithType :: String -- ^ input type (text, checkbox, ...)
              -> Html
inputWithType inputType = HE.input `HE.withAttrs` [HA.type_ inputType]

-- | Constructs a Html "input" tag with type and key.
inputWithTypeAndAts :: String -- ^ input type (text, checkbox, ...)
                    -> [HtmlAttr]
                    -> Html
inputWithTypeAndAts inputType ats = inputWithType inputType `HE.withAttrs` ats
