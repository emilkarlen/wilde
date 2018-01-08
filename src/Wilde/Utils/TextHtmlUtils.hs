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

-- | Utilities for Text.Html

module Wilde.Utils.TextHtmlUtils
       (
         renderClasses,
         withclasses,
         thead,
         tbody,
         tfoot,
         linkelem,
         thsForStrings,
         accesskey,
         ButtonLabel,
         ButtonType(..),
         button,
       )
       where

import Text.Html

-- |Makes the HTML class value of a list of classes.
renderClasses :: [String] -> String
renderClasses [] = ""
renderClasses cs = foldr1 (\x y -> x ++ " " ++ y) cs

-- |Adds the "class" attribute to an element, if there are any
-- given classes to add.
withclasses :: [String] -> Html -> Html
withclasses [] html = html
withclasses classes html = html ! [theclass (renderClasses classes)]

thead :: Html -> Html
thead = tag "THEAD"

tbody :: Html -> Html
tbody = tag "TBODY"

tfoot :: Html -> Html
tfoot = tag "TFOOT"

linkelem :: Html
linkelem = tag "LINK" noHtml

-- | Constructs TH elements for a list of header-info.
-- (header text,header classes)
thsForStrings :: [(String,
                   [String])
                 ]
              -> Html
thsForStrings xs = foldl (+++) noHtml $ map mkTh xs
    where mkTh (t,cs) = withclasses cs (th $ stringToHtml t)


-- | The accesskey attribute (of input controls).
accesskey :: Char -> HtmlAttr
accesskey ch = strAttr "accesskey" [ch]
  

-------------------------------------------------------------------------------
-- - button -
-------------------------------------------------------------------------------

type ButtonLabel = (String,Maybe Char) -- ^ Labe, maybe accesskey.

-- | All types of buttons.
data ButtonType = SubmitButtonType | ResetButtonType | PlainButtonType

buttonTypeHtmlAttr :: ButtonType -> HtmlAttr
buttonTypeHtmlAttr buttonType = HtmlAttr "type" typeStr
                                where
                                  typeStr = case buttonType of
                                    SubmitButtonType -> "submit"
                                    ResetButtonType  -> "reset"
                                    PlainButtonType  -> "button"

button :: ButtonType
          -> ButtonLabel
          -> Html
button buttonType (label,mbAccessKey) =
  let attrs = [buttonTypeHtmlAttr buttonType,
                value label] ++ accessKeyAttrs
      accessKeyAttrs = maybe [] (\ch -> [accesskey ch]) mbAccessKey
  in  input ! attrs
