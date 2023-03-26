module Wilde.WildeUi.WildeStyleType
       (
         module Wilde.GenericUi.Style,

         WildeStyle (..),
         WildeStyling(..),

         singleClassStyle,

         wildeStyling,

         wildeStyle,
         wildeStyled,

         withWildeStyle,
         withNeutralWildeStyle,

         withAdjustedStyled,
         withAdjustedStyle,

         wildeClassStyle,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.GenericUi.Style
import Wilde.Render.StyleForHtml
import Wilde.Utils.TextHtmlUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | The style representation used by Wilde.
newtype WildeStyle = WildeStyle
  {
    getClasses :: [ClassName]
  }
  deriving (Eq,Show)

singleClassStyle :: ClassName -> WildeStyle
singleClassStyle x = WildeStyle [x]

-- | A 'Styling' with 'WildeStyle'.
newtype WildeStyling a = WildeStyling (Styling WildeStyle a)

wildeStyling :: WildeStyle -> a -> WildeStyling a
wildeStyling style val = WildeStyling (Styling style val)

wildeStyle  :: WildeStyling a -> WildeStyle
wildeStyle (WildeStyling styling) = sStyle styling

wildeStyled :: WildeStyling a -> a
wildeStyled (WildeStyling styling) = sStyled styling

withWildeStyle :: WildeStyle -> a -> WildeStyling a
withWildeStyle ws x = WildeStyling $ Styling ws x

withAdjustedStyled :: WildeStyling a
                   -> (a -> a)
                   -> WildeStyling a
withAdjustedStyled (WildeStyling styling) f =
  WildeStyling (setStyled adjustedStyled styling)
  where
    adjustedStyled = f $ getStyled styling

withAdjustedStyle :: WildeStyling a
                  -> (WildeStyle -> WildeStyle)
                  -> WildeStyling a
withAdjustedStyle (WildeStyling styling) f =
  WildeStyling (setStyle adjustedStyle styling)
  where
    adjustedStyle = f $ getStyle styling

-- | Turns a 'VALUE' into a 'SVALUE' without style.
withNeutralWildeStyle :: a -> WildeStyling a
withNeutralWildeStyle = WildeStyling . Styling neutral

instance Semigroup WildeStyle where
    (WildeStyle xs) <> (WildeStyle ys) = WildeStyle (xs ++ ys)

instance Monoid WildeStyle where
    mempty = WildeStyle []

instance STYLE WildeStyle where

instance STYLE_FOR_HTML WildeStyle where
    applyStyleToHtml (WildeStyle classes) = withclasses classes

-- | Constructs a 'WildeStyle' consisting of a list of classes.
wildeClassStyle :: [ClassName] -> WildeStyle
wildeClassStyle = WildeStyle
