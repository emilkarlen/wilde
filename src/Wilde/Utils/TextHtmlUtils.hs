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
