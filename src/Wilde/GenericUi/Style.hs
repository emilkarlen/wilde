module Wilde.GenericUi.Style where


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Something that is a "style" (for example, CSS classes).
class Monoid s => STYLE s where
    -- neutral  :: s
    -- addStyle :: s -> s -> s -- ^ Adds two styles.

neutral :: STYLE s => s
neutral = mempty

-- | Adds two styles.
addStyle :: STYLE s => s -> s -> s
addStyle = mappend

-------------------------------------------------------------------------------
-- | Something that is styled: (c s a) is the type for an (a) styled with a
-- (s).
--
-- This class is for the \"container\", which can be used for any type of
-- style and any type of thing to style.
-------------------------------------------------------------------------------
class STYLING c where
    getStyle  :: c s a -> s          -- ^ Get the style.
    getStyled :: c s a -> a          -- ^ Get the styled value.
    setStyle  :: s -> c s a -> c s a -- ^ Sets the style.
    setStyled :: a -> c s a -> c s a -- ^ Sets the styled value.

addStyleToSTYLING :: (STYLE s,STYLING c) => s -> c s a -> c s a
addStyleToSTYLING s styled = let newStyle = addStyle (getStyle styled) s
                             in  setStyle newStyle styled

-- | A value of type a that is "styled" by a value of type s.
-- This is the "default" STYLING representation.
data Styling s a = Styling
    {
      sStyle  :: s,
      sStyled :: a
    }

-- | A styled value with neutral style.
withNeutralStyle :: STYLE s => a -> Styling s a
withNeutralStyle = Styling neutral

instance STYLING Styling where
    getStyle           = sStyle
    getStyled          = sStyled
    setStyle  s styled = styled { sStyle  = s }
    setStyled a styled = styled { sStyled = a }

-- | Type of individual "class" style values.
-- For HTML, a "class" is a "CSS class".
type ClassName = String
