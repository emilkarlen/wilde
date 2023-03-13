-- | Resources for the User Interface.
module Video.Ui.UiResources where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Render.Html.Attribute as HA
import qualified Wilde.Render.Html.Element as HE

import           Wilde.Media.WildeValue

import           Wilde.WildeUi.UiPrimitives

import qualified Video.Ui.UiResourcesRaw as UiResourcesRaw


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


image :: HE.URL -> ElementWithStyle
image url = SeHtml $ HE.image `HE.withAttrs` [HA.src url]

imageAnySV :: HE.URL -> AnySVALUE
imageAnySV = AnySVALUE . image


-------------------------------------------------------------------------------
-- - constants -
-------------------------------------------------------------------------------


imagePlayHtml = HE.image `HE.withAttrs` [HA.src UiResourcesRaw.play]

imagePlayASV,imagePlaySmallASV,imageDownASV,imageHourglassASV :: AnySVALUE

imagePlayASV       = imageAnySV UiResourcesRaw.play
imagePlaySmallASV  = imageAnySV UiResourcesRaw.play_s
imageDownASV       = imageAnySV UiResourcesRaw.down
imageHourglassASV  = imageAnySV UiResourcesRaw.hourglass

imageCreateASV,imageCreateFromASV,imageUpdateASV,imageDeleteASV :: AnySVALUE

imageCreateASV     = imageAnySV UiResourcesRaw.create
imageCreateFromASV = imageAnySV UiResourcesRaw.create_from
imageUpdateASV     = imageAnySV UiResourcesRaw.update
imageDeleteASV     = imageAnySV UiResourcesRaw.delete
