-- | Resources for the User Interface.
module Common.App.Ui.UiResources where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Text.Html as TH

import Wilde.Media.WildeValue

import Wilde.WildeUi.UiPrimitives

import qualified Common.App.Ui.UiResourcesRaw as UiResourcesRaw


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


image :: TH.URL -> ElementWithStyle
image url = SeHtml $ TH.image TH.! [TH.src url]

imageAnySV :: TH.URL -> AnySVALUE
imageAnySV = AnySVALUE . image


-------------------------------------------------------------------------------
-- - constants -
-------------------------------------------------------------------------------


imagePlayHtml = TH.image TH.! [TH.src UiResourcesRaw.play]

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
