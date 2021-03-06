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

         WwwLinkValue,
         wwwLinkValue,
         wwwLinkValueWithOnClick,
         
         Wilde.WildeUi.StdValueTypes.button,
         Button,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Word

import qualified Text.Html as Html

-- TODO : ta bort det här beroendet - formEncode.
import Network.CGI

import Wilde.Media.WildeValue

import Wilde.Render.Cgi.ServerVariables


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
  
  valueHtml   (BoolValueAsCheckBox b) = Html.input Html.! attrs
    where
      ro    = "readonly"
      attrs = [Html.thetype "checkbox",Html.HtmlAttr ro ro] ++ (if b then [Html.checked] else [])

instance SVALUE BoolValueAsCheckBox


-------------------------------------------------------------------------------
-- - IntValue -
-------------------------------------------------------------------------------


newtype IntValue = IntValue Int
    deriving (Eq,Show,Ord)

instance VALUE IntValue where
    valueHtml   (IntValue x) = Html.stringToHtml $ show x
    valueString (IntValue x) = show x

instance SVALUE IntValue


-------------------------------------------------------------------------------
-- - Word32Value -
-------------------------------------------------------------------------------


newtype Word32Value = Word32Value Word32
    deriving (Eq,Show,Ord)

instance VALUE Word32Value where
    valueHtml   (Word32Value x) = Html.stringToHtml $ show x
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
    valueHtml   (UnquotedStringValue x) = Html.stringToHtml x
    valueString (UnquotedStringValue x) = x

instance SVALUE UnquotedStringValue

quotedStringSvalue :: String -> AnySVALUE
quotedStringSvalue = AnySVALUE . QuotedStringValue

quotedStringValue :: String -> AnyVALUE
quotedStringValue = AnyVALUE . QuotedStringValue

instance VALUE QuotedStringValue where
    valueHtml   (QuotedStringValue x) = Html.primHtml x
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

layoutParagraphs :: [[String]] -> Html.Html
layoutParagraphs paras = foldl combine Html.noHtml unwrappedParas
  where
    unwrappedParas = map layoutPara paras
    combine wrapped unwrapped = wrapped Html.+++ (Html.paragraph unwrapped)

    layoutPara :: [String] -> Html.Html
    layoutPara lines = case map Html.stringToHtml lines of
      []     -> Html.noHtml
      (l:ls) -> l Html.+++ map (\html -> Html.br Html.+++ html) ls

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
newtype HrefValue = HrefValue (String,Html.URL) -- ^ Maybe link text, URL.

instance VALUE HrefValue where
  valueString (HrefValue (linkText,url)) = url
  valueHtml   (HrefValue (linkText,url)) = Html.anchor Html.! [Html.href url] $
                                           Html.stringToHtml linkText

instance SVALUE HrefValue


-------------------------------------------------------------------------------
-- - WwwLinkValue -
-------------------------------------------------------------------------------


data DomEvent = OnClick
                deriving (Show,Read,Eq,Ord,Enum,Bounded)

-- | A program in JavaScript.
type JavaScriptProgram = String

-- | Constructs a link without any DOM-events.
wwwLinkValue :: Html.URL -> ServerVariables -> a -> WwwLinkValue a
wwwLinkValue urlBase urlArgs display = WwwLinkValue urlBase urlArgs [] display

wwwLinkValueWithOnClick :: JavaScriptProgram
                        -> Html.URL
                        -> ServerVariables
                        -> a -> WwwLinkValue a
wwwLinkValueWithOnClick js urlBase urlArgs display = WwwLinkValue urlBase urlArgs [(OnClick,js)] display

data WwwLinkValue a = WwwLinkValue
                      {
                        linkUnencodedUrlBase :: Html.URL
                      , linkUnencodedUrlArgs :: ServerVariables
                      , linkEvents           :: [(DomEvent,JavaScriptProgram)]
                      , linkDisplay          :: a
                      }

instance VALUE a => VALUE (WwwLinkValue a) where
  valueString  (WwwLinkValue urlBase urlArgs domEvents display) = '[' : valueString display ++ "]"
  
  valueHtml    (WwwLinkValue urlBase urlArgs domEvents display) =
    Html.anchor (valueHtml display) Html.! (Html.href url : domEventAttrs)
    where
      url = if null urlArgs
            then urlBase
            else urlBase ++ "?" ++ (formEncode (map toMandatoryValue urlArgs))
      domEventAttrs = [Html.HtmlAttr (show event) jsPgm | (event,jsPgm) <- domEvents]

-- | If the value has a non-neutral style, then it is applied to the
-- HTML A element.
-- instance VALUE a => SVALUE (WwwLinkValue a)
instance SVALUE a => SVALUE (WwwLinkValue a)

data Button = Button String

button :: String  -- ^ label
       -> Button
button = Button

instance VALUE Button where
    valueHtml (Button label) = Html.submit "_" label

instance SVALUE Button
