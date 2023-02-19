-- | Components for laying out pages.
module Wilde.WildeUi.LayoutComponents
       (
         module Wilde.WildeUi.UiPrimitives,
         module Wilde.Media.WildeValue,

         verticalComponents,
         horizontalComponents,

         svalueComponent,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List

import qualified Wilde.Render.Html.Element as HE

import Wilde.WildeUi.UiPrimitives
import Wilde.Media.WildeValue


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - vertical -
-------------------------------------------------------------------------------


-- | Constructs a component that is a vertical list of sub components.
verticalComponents :: COMPONENT a => [a] -> AnyCOMPONENT
verticalComponents xs = AnyCOMPONENT $ Vertical xs

newtype Vertical a = Vertical [a]

instance COMPONENT a => COMPONENT (Vertical a) where
    componentHtml (Vertical xs) = HE.seq $ intersperse HE.br $ map componentHtml xs


-------------------------------------------------------------------------------
-- - horizontal -
-------------------------------------------------------------------------------


-- | Constructs a component that is a horizontal list of sub components.
horizontalComponents :: COMPONENT a => [a] -> AnyCOMPONENT
horizontalComponents xs = AnyCOMPONENT $ Horizontal xs

newtype Horizontal a = Horizontal [a]

instance COMPONENT a => COMPONENT (Horizontal a) where
    componentHtml (Horizontal xs) = HE.seq $ map componentHtml xs


-------------------------------------------------------------------------------
-- - svalueComponent -
-------------------------------------------------------------------------------

-- | A component that is a single 'SVALUE'.
svalueComponent :: SVALUE a => a -> AnyCOMPONENT
svalueComponent = AnyCOMPONENT . SvalueComponent

newtype SvalueComponent a = SvalueComponent a

instance VALUE a => VALUE (SvalueComponent a) where
  valueHtml   (SvalueComponent x) = valueHtml x
  valueString (SvalueComponent x) = valueString x

instance SVALUE a => SVALUE (SvalueComponent a)

instance SVALUE a => COMPONENT (SvalueComponent a) where
    componentHtml (SvalueComponent x) = valueHtml x
