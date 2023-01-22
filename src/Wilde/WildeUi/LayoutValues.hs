-- | Types used to layout values.
module Wilde.WildeUi.LayoutValues
       (
         horizontal,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Text.Html

import Wilde.Media.WildeValue


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


horizontal :: SVALUE a => [a] -> AnySVALUE
horizontal = AnySVALUE . Horizontal

newtype Horizontal a = Horizontal [a]

instance VALUE a => VALUE (Horizontal a) where
  valueHtml   (Horizontal []) = noHtml
  valueHtml   (Horizontal xs) = concatHtml $ map valueHtml xs
  -- valueHtml   (Horizontal xs) = concatHtml $ map valueHtmlStyled xs
  valueString (Horizontal xs) = concatMap valueString xs

instance SVALUE a => SVALUE (Horizontal a)
