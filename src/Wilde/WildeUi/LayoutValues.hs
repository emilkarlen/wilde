-- | Types used to layout values.
module Wilde.WildeUi.LayoutValues
       (
         horizontal,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Render.Html.Element as HE

import           Wilde.WildeUi.WildeValue


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


horizontal :: SVALUE a => [a] -> AnySVALUE
horizontal = AnySVALUE . Horizontal

newtype Horizontal a = Horizontal [a]

instance VALUE a => VALUE (Horizontal a) where
  valueHtml   (Horizontal []) = HE.empty
  valueHtml   (Horizontal xs) = HE.seq $ map valueHtml xs
  -- valueHtml   (Horizontal xs) = concatHtml $ map valueHtmlStyled xs
  valueString (Horizontal xs) = concatMap valueString xs

instance SVALUE a => SVALUE (Horizontal a)
