-- | Utilities for Html

module Wilde.Utils.TextHtmlUtils
       (
         renderClasses,
         withclasses,
         thsForStrings,
         ButtonLabel,
         ButtonType(..),
         button,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Render.Html.Types
import qualified Wilde.Render.Html.Attribute as HA
import qualified Wilde.Render.Html.Element as HE


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- |Makes the HTML class value of a list of classes.
renderClasses :: [String] -> String
renderClasses [] = ""
renderClasses cs = foldr1 (\x y -> x ++ " " ++ y) cs

-- |Adds the "class" attribute to an element, if there are any
-- given classes to add.
withclasses :: [String] -> Html -> Html
withclasses [] html = html
withclasses classes html = html `HE.withAttrs` [HA.class_ (renderClasses classes)]

-- | Constructs TH elements for a list of header-info.
-- (header text,header classes)
thsForStrings :: [(String,[String])]
              -> Html
thsForStrings xs = HE.seq $ map mkTh xs
    where
      mkTh (t,cs) = withclasses cs (HE.th $ HE.str t)


-------------------------------------------------------------------------------
-- - button -
-------------------------------------------------------------------------------

type ButtonLabel = (String,Maybe Char) -- ^ Labe, maybe accesskey.

-- | All types of buttons.
data ButtonType = SubmitButtonType | ResetButtonType | PlainButtonType

buttonTypeHtmlAttr :: ButtonType -> HtmlAttr
buttonTypeHtmlAttr buttonType = HA.type_ typeStr
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
                HA.value label] ++ accessKeyAttrs
      accessKeyAttrs = maybe [] (\ch -> [HA.accesskey ch]) mbAccessKey
  in  HE.input `HE.withAttrs` attrs
