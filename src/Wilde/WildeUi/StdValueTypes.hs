-- | Some common value types.
module Wilde.WildeUi.StdValueTypes
       (
         module Wilde.Media.WildeValue,

         unquotedStringSvalue,
         unquotedStringValue,
         quotedStringSvalue,
         quotedStringValue,

         BoolValueAsCheckBox(..),
         IntValue(..),
         Word32Value(..),
         UnquotedStringValue(..),
         QuotedStringValue(..),
         UnquotedMultiLineTextValue(..),
         HrefValue(..),

         JavaScriptProgram,

         HRef(..),

         -- * WWW link (do not use!)
         WwwLinkValue,
         wwwLinkValue,
         wwwLinkValueWithOnClick,

         -- * WWW link, improved, with label that cannot be a link
         LinkLabel(..),
         WwwLinkValueDoneRight(..),
         wwwLinkValue_dr,
         wwwLinkValueWithOnClick_dr,

         -- * Button (used??)

         Button(..),
         button,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Word

import           Wilde.Render.Html.Types
import qualified Wilde.Render.Html.Attribute as HA
import qualified Wilde.Render.Html.Element as HE

import Network.HTTP.Base (urlEncodeVars)

import Wilde.Media.WildeValue

import Wilde.Render.Cgi.ServerVariables
import Wilde.Render.Cgi.HRef

import Wilde.Render.StyleForHtml (STYLE_FOR_HTML(..))
import Data.List (intersperse)

-------------------------------------------------------------------------------
-- - ...Value -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - BoolValue -
-------------------------------------------------------------------------------


-- | A boolean value that is rendered as a check box in HTML.
newtype BoolValueAsCheckBox = BoolValueAsCheckBox Bool

instance VALUE BoolValueAsCheckBox where
  valueString (BoolValueAsCheckBox b) = if b then "T" else "F"

  valueHtml   (BoolValueAsCheckBox b) = HE.input `HE.withAttrs` attrs
    where
      attrs = [HA.type_ "checkbox",HA.readonly] ++ (if b then [HA.checked] else [])

instance SVALUE BoolValueAsCheckBox


-------------------------------------------------------------------------------
-- - IntValue -
-------------------------------------------------------------------------------


newtype IntValue = IntValue Int
    deriving (Eq,Show,Ord)

instance VALUE IntValue where
    valueHtml   (IntValue x) = HE.str $ show x
    valueString (IntValue x) = show x

instance SVALUE IntValue


-------------------------------------------------------------------------------
-- - Word32Value -
-------------------------------------------------------------------------------


newtype Word32Value = Word32Value Word32
    deriving (Eq,Show,Ord)

instance VALUE Word32Value where
    valueHtml   (Word32Value x) = HE.str $ show x
    valueString (Word32Value x) = show x

instance SVALUE Word32Value


-------------------------------------------------------------------------------
-- - ...StringValue -
-------------------------------------------------------------------------------


unquotedStringSvalue :: String -> AnySVALUE
unquotedStringSvalue = AnySVALUE . UnquotedStringValue

unquotedStringValue :: String -> AnyVALUE
unquotedStringValue = AnyVALUE . UnquotedStringValue

newtype UnquotedStringValue = UnquotedStringValue String
    deriving (Eq,Show)

newtype QuotedStringValue = QuotedStringValue String
    deriving (Eq,Show)

instance VALUE UnquotedStringValue where
    valueHtml   (UnquotedStringValue x) = HE.str x
    valueString (UnquotedStringValue x) = x

instance SVALUE UnquotedStringValue

quotedStringSvalue :: String -> AnySVALUE
quotedStringSvalue = AnySVALUE . QuotedStringValue

quotedStringValue :: String -> AnyVALUE
quotedStringValue = AnyVALUE . QuotedStringValue

instance VALUE QuotedStringValue where
    valueHtml   (QuotedStringValue x) = HE.htmlString x
    valueString (QuotedStringValue x) = x

instance SVALUE QuotedStringValue


-------------------------------------------------------------------------------
-- - UnquotedMultiLineTextValue -
-------------------------------------------------------------------------------


-- | Text that should be layed out:
-- paragraphs are separated by two or more new-lines.
-- A single new-line represents a line-break.
newtype UnquotedMultiLineTextValue = UnquotedMultiLineTextValue String

instance VALUE UnquotedMultiLineTextValue where
  valueString (UnquotedMultiLineTextValue x) = x
  valueHtml   (UnquotedMultiLineTextValue x) = layoutParagraphs paras
    where
      paras = paraAndLineBreakSplit x


instance SVALUE UnquotedMultiLineTextValue

layoutParagraphs :: [[String]] -> Html
layoutParagraphs paras = foldl combine HE.empty unwrappedParas
  where
    unwrappedParas = map layoutPara paras
    combine wrapped unwrapped = HE.seq [wrapped, HE.paragraph unwrapped]

    layoutPara :: [String] -> Html
    layoutPara lines = case map HE.str lines of
      []     -> HE.empty
      ls     -> HE.seq $ intersperse HE.br ls

-- | Each element of the result is a paragraph.
-- The elements in a paragraph are separated by line-breaks.
paraAndLineBreakSplit :: String -> [[String]]
paraAndLineBreakSplit s = splitParas [] (dropWhileNewline s)
  where
    splitParas :: [[String]] -> String -> [[String]]
    splitParas paras [] = reverse paras
    splitParas paras xs = splitParas (para : paras) rest
      where
        (para,rest) = splitLines [] xs

    -- (accumulated lines -> next-line-start) -> (lines,empty/does not begin with NL)
    splitLines :: [String] -> String -> ([String],String)
    splitLines lines [] = (lines,[])
    splitLines lines xs =
      case nlSepa of
        []           -> (reverse lines',[])
        (nl1:nl2:xs) -> (reverse lines',afterSepa)
        [nl]         -> splitLines lines' afterSepa
      where
        (line,rest) = splitOnNextNewline xs -- rest: empty or starts with nl
        lines'      = line : lines
        (nlSepa,afterSepa) = splitOnNextNonNewline rest

    dropWhileNewline      = dropWhile (=='\n')
    splitOnNextNewline    = span (/='\n')
    splitOnNextNonNewline = span (=='\n')



-------------------------------------------------------------------------------
-- - HrefValue -
-------------------------------------------------------------------------------


-- | A Hypertext reference, with a text to display and the target URL.
newtype HrefValue = HrefValue (String,URL) -- ^ link text, URL.

instance VALUE HrefValue where
  valueString (HrefValue (linkText,url)) = url
  valueHtml   (HrefValue (linkText,url)) =
    HE.anchor (HE.str linkText) `HE.withAttrs` [HA.href url]


instance SVALUE HrefValue


-------------------------------------------------------------------------------
-- - WwwLinkValue -
-------------------------------------------------------------------------------


-- | Constructs a link without any DOM-events.
wwwLinkValue :: HRef -> a -> WwwLinkValue a
wwwLinkValue href display = WwwLinkValue href [] display

wwwLinkValueWithOnClick :: JavaScriptProgram
                        -> HRef
                        -> a -> WwwLinkValue a
wwwLinkValueWithOnClick js href display = WwwLinkValue href [(OnClick,js)] display

data WwwLinkValue a = WwwLinkValue
  {
    linkHref    :: HRef
  , linkEvents  :: [(DomEvent,JavaScriptProgram)]
  , linkDisplay :: a
  }

instance VALUE a => VALUE (WwwLinkValue a) where
  valueString  (WwwLinkValue (HRef urlBase urlArgs) domEvents display) = '[' : valueString display ++ "]"

  valueHtml    (WwwLinkValue href domEvents display) =
    HE.anchor (valueHtml display) `HE.withAttrs` (HA.href (hrefToUrl href) : domEventAttrs)
    where
      domEventAttrs = [HA.domEvent event jsPgm | (event,jsPgm) <- domEvents]

-- | If the value has a non-neutral style, then it is applied to the
-- HTML A element.
-- instance VALUE a => SVALUE (WwwLinkValue a)
instance SVALUE a => SVALUE (WwwLinkValue a)


-------------------------------------------------------------------------------
-- - WwwLinkValueDoneRight -
-------------------------------------------------------------------------------


-- | The visual element of a link.
data LinkLabel
    = TextLabel  String -- ^ Text label
    | ImageLabel String -- ^ Path of an image file

-- | Constructs a link without any DOM-events.
wwwLinkValue_dr :: HRef -> WildeStyling LinkLabel -> WwwLinkValueDoneRight
wwwLinkValue_dr href display = WwwLinkValueDoneRight href [] display

wwwLinkValueWithOnClick_dr :: JavaScriptProgram
                          -> HRef
                          -> WildeStyling LinkLabel
                          -> WwwLinkValueDoneRight
wwwLinkValueWithOnClick_dr js href display = WwwLinkValueDoneRight href [(OnClick,js)] display

data WwwLinkValueDoneRight = WwwLinkValueDoneRight
  {
    linkDrHref    :: HRef
  , linkDrEvents  :: [(DomEvent,JavaScriptProgram)]
  , linkDrDisplay :: WildeStyling LinkLabel
  }

instance VALUE WwwLinkValueDoneRight where
  valueString (WwwLinkValueDoneRight {linkDrHref = href}) = hrefToUrl href

  valueHtml   (WwwLinkValueDoneRight href domEvents display) =
    HE.anchor labelHtml `HE.withAttrs` (HA.href (hrefToUrl href) : domEventAttrs)
    where
      labelHtml     = label2Html (wildeStyled display)
      domEventAttrs = [HA.custom (show event) jsPgm | (event,jsPgm) <- domEvents]

label2Html :: LinkLabel -> Html
label2Html (TextLabel  text)      = HE.str text
label2Html (ImageLabel imagePath) = HE.image `HE.withAttrs` [HA.src imagePath]

hrefToUrl :: HRef -> String
hrefToUrl (HRef urlBase urlArgs) =
  if null urlArgs
  then urlBase
  else urlBase ++ "?" ++ urlEncodeVars (map toMandatoryValue urlArgs)

-- | If the value has a non-neutral style, then it is applied to the
-- HTML A element.
-- instance VALUE a => SVALUE (WwwLinkValue a)
instance SVALUE WwwLinkValueDoneRight where
  valueStyle = wildeStyle . linkDrDisplay

  valueHtmlStyled x@(WwwLinkValueDoneRight {linkDrDisplay = display} ) =
    applyStyleToHtml style html
    where
      html  = valueHtml x
      style = wildeStyle display

  valueHtmlStyledWrapper _ = Nothing


-------------------------------------------------------------------------------
-- - WwwLinkValueDoneRight -
-------------------------------------------------------------------------------


newtype Button = Button String

button :: String  -- ^ label
       -> Button
button = Button

instance VALUE Button where
    valueHtml (Button label) = HE.submit "_" label

instance SVALUE Button
