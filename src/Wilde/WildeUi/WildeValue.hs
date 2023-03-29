-- | Classes and datatypes for encapsulating data
-- that is presented in the user interface.

{-# LANGUAGE ExistentialQuantification #-}

module Wilde.WildeUi.WildeValue
  ( module Wilde.GenericUi.Value,
    module Wilde.WildeUi.WildeStyle,
    AnyVALUE (..),
    SVALUE (..),
    AnySVALUE (..),
    svalueEmpty,
    withNeutralStyleAny,
    anySvalue2Value,
    ValueWithHiddenStyle (..),
    hideStyle,
    hideStyleAny,
    defaultHtmlStyled,
  )
where

-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import           Wilde.Render.Html.Types ( Html )
import qualified Wilde.Render.Html.Element as HE
import qualified Wilde.Render.Html.Utils as HU
import           Wilde.Render.StyleForHtml

import           Wilde.GenericUi.Value
import           Wilde.WildeUi.WildeStyle

-------------------------------------------------------------------------------
-- - SVALUE -
-------------------------------------------------------------------------------

-- | A VALUE styled by a WildeStyle.
--
-- The methods of VALUE gives the unstyled value.
--
-- 11-09-03: Shouldn't this class be parametrized by the style representation?
-- Now it seems like Wilde-stuff appears at a level above Wilde.
class VALUE a => SVALUE a where
  -- | Style for this value.
  valueStyle :: a -> WildeStyle

  -- | Default style: neutral.
  valueStyle _ = neutral

  -- | Outputs the value as styled according to the objects style.
  valueHtmlStyled :: a -> Html

  -- | Gives the HTML element that should carry the style.
  -- If 'Nothing', then the style is put on the root element,
  -- or, if there are a list of elements at the top level,
  -- a SPAN that encapsulates all these elements.
  valueHtmlStyledWrapper :: a -> Maybe (Html -> Html)
  valueHtmlStyledWrapper _ = Nothing

  valueHtmlStyled x = defaultHtmlStyled (valueStyle x) (valueHtml x) (valueHtmlStyledWrapper x)

-- | Adds style to html.
defaultHtmlStyled ::
  WildeStyle ->
  -- ^ Style to add (may be none=neutral)
  Html ->
  -- ^ Unstyled html
  Maybe (Html -> Html) ->
  -- ^ See `SVALUE`.
  Html
defaultHtmlStyled style unstyledHtml wrapper
 | style == neutral = unstyledHtml
 | otherwise =
    maybe HE.empty (applyStyleToHtml style) htmlWRootElem
    where
      htmlWRootElem :: Maybe Html
      htmlWRootElem = HU.rootElemOrEmpty HE.span html

      html :: Html
      html = case wrapper of
        Nothing -> unstyledHtml
        Just f  -> f unstyledHtml

instance VALUE a => VALUE (WildeStyling a) where
  valueHtml (WildeStyling styling) = (valueHtml . getStyled) styling

instance VALUE a => SVALUE (WildeStyling a) where
  valueStyle (WildeStyling styling) = getStyle styling


-------------------------------------------------------------------------------
-- - ValueWithHiddenStyle -
-------------------------------------------------------------------------------

-- | A 'VALUE' constructed from a 'SVALUE', with styling preserved.
-- The primary purpose of this type is to be able to preserve the style of
-- 'SVALUE's of each individual element of a list.
-- The style should be included in
-- HTML even when it is "generated" using 'VALUE's 'valueHtml'.
-- (Problem: 'VALUE's 'valueHtml' has no knowledge of style (since it is
-- located in 'VALUE') so it cannot generate styled HTML).  This solution
-- uses a new type, so that we can implement 'valueHtml', and this type
-- stores a list of 'SVALUE's, so it knows about style.)
--
-- Here is an example of how a list with two individually styled elements can be constructed:
-- From right to left, we go from 'VALUE' to 'SVALUE' and back to 'VALUE' again.
-- (Note: UnquotedStringValue constructs a 'SVALUE', but that is also a 'VALUE', and it is that
--  role it has here.)
-- Then, in the "in" we make the list first a 'SVALUE' (withNeutralWildeStyle) and then a 'SVALUE' (AnySVALUE).
-- let v1 = ValueWithHiddenStyle $ withWildeStyle (WildeStyle [MyStyle.posClass]) (UnquotedStringValue "v1")
--     v2 = ValueWithHiddenStyle $ withNeutralWildeStyle                              (UnquotedStringValue "v2")
--     v3 = ValueWithHiddenStyle $ withWildeStyle (WildeStyle [MyStyle.negClass]) (UnquotedStringValue "v3")
-- in  AnySVALUE $ withNeutralWildeStyle [v1,v2,v3]
data ValueWithHiddenStyle = forall a. SVALUE a => ValueWithHiddenStyle a

instance VALUE ValueWithHiddenStyle where
  valueHtml (ValueWithHiddenStyle svalue) = valueHtmlStyled svalue

-- | Hide any style associated with the given 'SVALUE' by fixing it, tranforming it to
-- a 'VALUE'.  The style becomes hidden for its surroundings, but it is still there,
-- and cannot be changed.
hideStyle :: SVALUE a => a -> ValueWithHiddenStyle
hideStyle = ValueWithHiddenStyle

hideStyleAny :: SVALUE a => a -> AnyVALUE
hideStyleAny = AnyVALUE . hideStyle

-- BEGIN experiment (används inte, men känns vettig) (stötte på
-- typproblem, som dock inte verkar betyda att lösningen är kass).
data Value
  = forall a. SVALUE a => ValueWithHiddenStyle2 {fixStyle :: a}
  | forall a. VALUE a => ValueOfValue {anyValue :: a}

instance VALUE Value where
  valueHtml (ValueWithHiddenStyle2 sval) = valueHtmlStyled sval
  valueHtml (ValueOfValue val) = valueHtml val

-- END experiment

-------------------------------------------------------------------------------
-- - AnySVALUE -
-------------------------------------------------------------------------------

-- | Type for heterougenous lists of styled values.
--
-- This is the "Greatest Common Divisor" of the database, representation and
-- presentation layers.
-- But ... it is unfortunate that it is not 'AnyVALUE' that is the GCD; it
-- should be! The reason is that the database layer should not need to be
-- aware of styling (since styling is only for presentation).  At the moment,
-- we must use this type, since value-styling is tied to the representation.
data AnySVALUE = forall a. SVALUE a => AnySVALUE a

instance Show AnySVALUE where
  show = valueString

instance VALUE AnySVALUE where
  valueHtml (AnySVALUE x) = valueHtml x

instance SVALUE AnySVALUE where
  valueStyle (AnySVALUE x) = valueStyle x
  valueHtmlStyled (AnySVALUE x) = valueHtmlStyled x
  valueHtmlStyledWrapper (AnySVALUE x) = valueHtmlStyledWrapper x

instance SVALUE a => SVALUE (Maybe a) where
  valueStyle Nothing = neutral
  valueStyle (Just x) = valueStyle x

instance (SVALUE a, SVALUE b) => SVALUE (Either a b) where
  valueStyle (Left x) = valueStyle x
  valueStyle (Right y) = valueStyle y

instance SVALUE ValueEmpty where
  valueStyle _ = neutral

instance EMPTY AnySVALUE where
  empty = AnySVALUE $ ValueEmpty ()

-- | Shortcut to avoid type clashes with empty :: AnyVALUE.
svalueEmpty :: AnySVALUE
svalueEmpty = empty

-- | Transforms an AnySVALUE to an AnyVALUE.
anySvalue2Value :: AnySVALUE -> AnyVALUE
anySvalue2Value (AnySVALUE x) = AnyVALUE x

-- | Transforms a 'VALUE' to a 'SVALUE'.
--
-- Sets the style to neutral.
withNeutralStyleAny :: VALUE a => a -> AnySVALUE
withNeutralStyleAny v = AnySVALUE $ WithNeutralStyle v

newtype WithNeutralStyle a = WithNeutralStyle a

instance VALUE a => VALUE (WithNeutralStyle a) where
  valueHtml (WithNeutralStyle x) = valueHtml x

instance VALUE a => SVALUE (WithNeutralStyle a)
