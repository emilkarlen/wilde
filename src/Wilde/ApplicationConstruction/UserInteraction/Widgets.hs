-- | Implementation of some common widgets.
--
-- Also some tools for constructing widgets.
module Wilde.ApplicationConstruction.UserInteraction.Widgets
       (
         -- * Re-exporting

         WIDGET(..),
         AnyWIDGET(..),

         -- * Widget with key

         WithWidgetKey(..),

         newWidgetWithKey,
         newAnyWidgetWithKey,

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


import Data.Maybe

import qualified Text.Html as H

import Wilde.GenericUi.Widget
import Wilde.GenericUi.Value

import Wilde.Media.Element
import Wilde.Media.UserInteraction (LabelAndWidget)

import Wilde.Media.GenericStringRep


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

newAnyWidgetWithKey :: WIDGET widgetInfo
                    => ElementKey
                    -> widgetInfo
                    -> AnyWIDGET
newAnyWidgetWithKey (objectName,attributeName) theWidgetInfo =
  AnyWIDGET $ newWidgetWithKey (objectName,attributeName) theWidgetInfo

-------------------------------------------------------------------------------
-- | Constructs a 'LabelAndWidget' with the same
-- element key in the label and widget.
-------------------------------------------------------------------------------
mkLabelAndWidgetFromInfo :: WIDGET widgetInfo
                         => ElementKey
                         -> String
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
       thevalue   = maybe Nothing (Just . H.value) theDefault
       thesize    = Just $ H.size (show theSize)
       attributes = [thevalue,thesize] :: [Maybe H.HtmlAttr]


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
    (H.textarea thevalue) H.! attributes
     where
       thevalue   = maybe H.noHtml H.stringToHtml theDefault
       cols       = H.cols (show width)  :: H.HtmlAttr
       rows       = H.rows (show height) :: H.HtmlAttr
       attributes = [cols,rows]  :: [H.HtmlAttr]


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
    H.input H.! ([atType,atValue] ++ atChecked)
     where
       atType    = H.thetype "checkbox"
       atValue   = H.value theValue :: H.HtmlAttr
       atChecked = if theDefault then [H.checked] else [] :: [H.HtmlAttr]


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
    H.select (H.concatHtml theOptions)
     where
       mkOption   = maybe optionWithoutDefault optionWithDefault mbDefault
       theOptions = map mkOption options

       -- An option in a selection list, when there is no default specified.
       optionWithoutDefault :: (String,AnyVALUE) -> H.Html
       optionWithoutDefault (value,pres) = H.option (valueHtml pres) H.! [H.value value]

       -- An option in a selection list, when there is a default specified.
       optionWithDefault :: GenericStringRep -> (String,AnyVALUE) -> H.Html
       optionWithDefault defaultValue (value,pres) =
         let
           withoutDefault = H.option (valueHtml pres) H.! [H.value value]
         in
          if (defaultValue == value)
          then withoutDefault H.! [H.selected]
          else withoutDefault


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


htmlForWithWidgetKey :: WithWidgetKey a
                     -> (a -> H.Html)
                     -> H.Html
htmlForWithWidgetKey (WithWidgetKey ek widgetInfo) renderWidgetInfo =
  renderWidgetInfo widgetInfo H.! [keyAttribute]
  where
    keyAttribute = H.name $ elementKeyRender ek :: H.HtmlAttr

-- | Constructs a Html "input" tag with type and key.
inputWithType :: String -- ^ input type (text, checkbox, ...)
              -> H.Html
inputWithType inputType = H.input H.! [H.thetype inputType]

-- | Constructs a Html "input" tag with type and key.
inputWithTypeAndAts :: String -- ^ input type (text, checkbox, ...)
                    -> [H.HtmlAttr]
                    -> H.Html
inputWithTypeAndAts inputType ats = inputWithType inputType H.! ats
