module Video.Ui.LinkRenderer
(
  standardServiceLinkRenderer,

  -- * The standard buttons for some services
  serviceLink_button_ShowOne,
  serviceLink_button_CreateOne,
  serviceLink_button_CreateOneFrom,
  serviceLink_button_UpdateOne,
  serviceLink_button_DeleteOne,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.WildeUi.StdValueTypes

import           Wilde.Render.ServiceLink

import qualified Video.Ui.UiResources as UiResources

import           Wilde.ApplicationConstruction.StandardServices


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


standardServiceLinkRenderer :: StandardServiceLinkRenderer
standardServiceLinkRenderer serviceEnum =
  renderServiceLink_svalue display
  where
    display :: AnySVALUE
    display = case serviceEnum of
      (StandardObjectTypeService otServiceEnum) -> otServiceDisplay otServiceEnum
      (StandardObjectService      oServiceEnum) ->  oServiceDisplay  oServiceEnum

otServiceDisplay :: StandardObjectTypeServiceEnum -> AnySVALUE
otServiceDisplay enum =
  case enum of
    CreateOne     -> UiResources.imageCreateASV
    ShowMany      -> AnySVALUE $ UnquotedStringValue "Show list"
    ShowSelection -> AnySVALUE $ UnquotedStringValue "Show selection"

oServiceDisplay :: StandardObjectServiceEnum -> AnySVALUE
oServiceDisplay enum =
  case enum of
    CreateOneFrom -> UiResources.imageCreateFromASV
    ShowOne       -> UiResources.imagePlayASV
    UpdateOne     -> UiResources.imageUpdateASV
    DeleteOne     -> UiResources.imageDeleteASV


serviceLink_button_ShowOne :: ServiceLink -> AnySVALUE
serviceLink_button_ShowOne serviceLink =
  renderServiceLink_svalue UiResources.imagePlayASV serviceLink

serviceLink_button_CreateOne :: ServiceLink -> AnySVALUE
serviceLink_button_CreateOne serviceLink =
  renderServiceLink_svalue UiResources.imageCreateASV serviceLink

serviceLink_button_CreateOneFrom :: ServiceLink -> AnySVALUE
serviceLink_button_CreateOneFrom serviceLink =
  renderServiceLink_svalue UiResources.imageCreateFromASV serviceLink

serviceLink_button_UpdateOne :: ServiceLink -> AnySVALUE
serviceLink_button_UpdateOne serviceLink =
  renderServiceLink_svalue UiResources.imageUpdateASV serviceLink

serviceLink_button_DeleteOne :: ServiceLink -> AnySVALUE
serviceLink_button_DeleteOne serviceLink =
  renderServiceLink_svalue UiResources.imageDeleteASV serviceLink
