-- | Functionality for rendering User Interaction in terms of COMPONENT.
--
-- The idea is that 'ObjectModelUserInteraction' should not need
-- knowledge of output medias - it should be "rendition independent".

module Wilde.Render.UserInteractionRendering
       (
         formBlockComponent,
         formComponent,
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Text.Html as H

import qualified Wilde.Media.UserInteraction.Output as UiOm
import           Wilde.Media.UserInteraction as UI
import qualified Wilde.Media.WildeStyle as WS

import           Wilde.WildeUi.TableUtils
import           Wilde.WildeUi.WildeComponent as WC
import qualified Wilde.WildeUi.StdValueTypes
import           Wilde.WildeUi.LayoutComponents

import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo

import qualified Wilde.Application.Service.PopUp as PopUp


-------------------------------------------------------------------------------
-- - formBlockComponent -
-------------------------------------------------------------------------------


-- | Constructs an 'AnyCOMPONENT' for a 'FormBlock'.

formBlockComponent :: FormBlock -> AnyCOMPONENT
formBlockComponent (FormBlock interaction metaValues)  =
    let
      formContents   = [AnyCOMPONENT $ HtmlOnly metaValuesHtml
                       ,AnyCOMPONENT $ TableListComponent Nothing styledTable]
      styledTable    = addStyleToSTYLING WS.userInteractionTable wildeTable
      wildeTable    :: WildeTable
      wildeTable     = wildeHeaderValueTable
                       renderHeader
                       renderValue
                       WS.weAttribute WS.tableColumnStylesInputOne
                       interaction

      metaValuesHtml = H.concatHtml $ map metaValueInput metaValues
    in
     verticalComponents formContents
  where
    renderHeader = either
                   (withNeutralStyleAny . fst)
                   (withNeutralStyleAny . Wilde.WildeUi.StdValueTypes.UnquotedStringValue . fst)
    renderValue  = either
                   (withNeutralStyleAny . snd)
                   snd


-------------------------------------------------------------------------------
-- - formComponent -
-------------------------------------------------------------------------------


-- | Constructs an 'AnyCOMPONENT' for a 'Form'.

formComponent :: Form
              -> UiOm.UserInteractionOutputMonad AnyCOMPONENT
formComponent form =
  do

    custEnv             <- UiOm.getCustomEnvironment
    buttonTexter        <- UiOm.getEnvs UiOm.envButtonTexter
    let custEnvCgiVals   = ElementSetIo.customEnvironmentSetToCgiValues custEnv
    let custEnvHtml      = H.concatHtml $ map cgiValueInput custEnvCgiVals
    let formMetasHtml    = H.concatHtml $ map metaValueInput (formMetaValues form)
    let allMetasHtml     = H.concatHtml [formMetasHtml,custEnvHtml]
    let formContents     = [AnyCOMPONENT $ HtmlOnly allMetasHtml,
                            verticalComponents $ (map formBlockComponent (formBlocks form)),
                            AnyCOMPONENT $ FormButtons (buttonTexter PopUp.Ok) (buttonTexter PopUp.Reset)]
    return $
      AnyCOMPONENT $ FormComponent
                    {
                      WC.formAction  = UI.formAction form,
                      WC.formMethod  = Get,
                      WC.formContent = formContents
                     }


-------------------------------------------------------------------------------
-- - Label -
-------------------------------------------------------------------------------


instance VALUE Label where
    valueHtml label = H.stringToHtml $ labelString label


-------------------------------------------------------------------------------
-- - Widget -
-------------------------------------------------------------------------------



-- | Constructs a Html "input" tag with type and key.
inputWithTypeAndKey :: String -- ^ input type (text, checkbox, ...)
                    -> ElementKey
                    -> H.Html
inputWithTypeAndKey inputType key = withKey key $ H.input H.! [H.thetype inputType]

-- | Adds a key to the given Html.
withKey :: ElementKey -> H.Html -> H.Html
withKey key html = html H.! [H.name (elementKeyRender key)]


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------

-- | A HTML "input" element for a Meta Element.
metaValueInput :: Element -> H.Html
metaValueInput (elementKey,value) = H.input H.! [H.thetype "hidden",
                                                 H.name (elementKeyRender elementKey),
                                                 H.value value]

-- | A HTML "input" element for a CGI value.
cgiValueInput :: (String,Maybe String) -> H.Html
cgiValueInput (key,Just value) = H.input H.! [H.thetype "hidden",
                                              H.name key,
                                              H.value value]
cgiValueInput (key,Nothing) = H.input H.! [H.thetype "hidden",
                                           H.name key]
