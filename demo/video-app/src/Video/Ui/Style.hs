module Video.Ui.Style where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.WildeMedia


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


withSingleClassStyle :: ClassName -> Title -> WildeStyling Title
withSingleClassStyle c t = wildeStyling (singleClassStyle c) t
