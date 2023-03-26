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


import           Wilde.Render.Html.Types hiding (Label)
import qualified Wilde.Render.Html.Element as HE
import qualified Wilde.Render.Html.Attribute as HA


import qualified Wilde.Media.UserInteraction.Output as UiOm
import           Wilde.Media.UserInteraction as UI
import           Wilde.Media.UserInteraction.Output (PopUpButtonTexter)

import qualified Wilde.WildeUi.WildeStyle as WS
import           Wilde.WildeUi.WildeTables
import           Wilde.WildeUi.WildeComponent as WC
import qualified Wilde.WildeUi.StdValueTypes
import           Wilde.WildeUi.WildeTable
import           Wilde.WildeUi.LayoutComponents as LayoutComponents

import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo
import qualified Wilde.ApplicationConstruction.Presentation.DataAndButtonsComponent as DataAndButtons

import qualified Wilde.Application.Service.PopUp as PopUp


-------------------------------------------------------------------------------
-- - formBlockComponent -
-------------------------------------------------------------------------------


-- | Constructs an 'AnyCOMPONENT' for a 'FormBlock'.

formBlockComponent :: FormBlock -> AnyCOMPONENT
formBlockComponent (FormBlock interaction metaValues)  =
    let
      formContents   = [AnyCOMPONENT $ HtmlOnly metaValuesHtml
                       ,AnyCOMPONENT $ TableListComponent styledTable]
      styledTable    = addStyleToSTYLING WS.userInteractionTable wildeTable
      wildeTable    :: WildeTable
      wildeTable     = wildeHeaderValueTable
                       renderHeader
                       renderValue
                       WS.weAttribute WS.tableColumnStylesInputOne
                       interaction

      metaValuesHtml = HE.seq $ map metaValueInput metaValues
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
    let custEnvHtml      = HE.seq $ map cgiValueInput custEnvCgiVals
    let formMetasHtml    = HE.seq $ map metaValueInput (formMetaValues form)
    let allMetasHtml     = HE.seq [formMetasHtml,custEnvHtml]
    let formContents     = [ AnyCOMPONENT $ HtmlOnly allMetasHtml
                           , dataAndButtons (formBlocks form) buttonTexter
                           ]
    pure $
      AnyCOMPONENT $ FormComponent
                    {
                      WC.formAction  = UI.formAction form,
                      WC.formMethod  = Get,
                      WC.formContent = formContents
                     }
  where
    dataAndButtons :: [FormBlock] -> PopUpButtonTexter -> AnyCOMPONENT
    dataAndButtons formBlocks buttonTexter =
      DataAndButtons.new formBlocksC (buttons buttonTexter)
      where
        formBlocksC :: AnyCOMPONENT
        formBlocksC  = verticalComponents $ map formBlockComponent formBlocks

    buttons :: PopUpButtonTexter -> [AnySVALUE]
    buttons buttonTexter =
      [ WC.formButton WC.Reset  (buttonTexter PopUp.Reset)
      , WC.formButton WC.Submit (buttonTexter PopUp.Ok)
      ]


-------------------------------------------------------------------------------
-- - Label -
-------------------------------------------------------------------------------


instance VALUE Label where
    valueHtml label = HE.str $ labelString label


-------------------------------------------------------------------------------
-- - Widget -
-------------------------------------------------------------------------------


-- | Constructs a Html "input" tag with type and key.
inputWithTypeAndKey :: String -- ^ input type (text, checkbox, ...)
                    -> ElementKey
                    -> Html
inputWithTypeAndKey inputType key = withKey key $ HE.input `HE.withAttrs` [HA.type_ inputType]

-- | Adds a key to the given Html.
withKey :: ElementKey -> Html -> Html
withKey key html = html `HE.withAttrs` [HA.name (elementKeyRender key)]


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------

-- | A HTML "input" element for a Meta Element.
metaValueInput :: Element -> Html
metaValueInput (elementKey,value) =
  HE.input `HE.withAttrs` [HA.type_hidden,
                           HA.name (elementKeyRender elementKey),
                           HA.value value]

-- | A HTML "input" element for a CGI value.
cgiValueInput :: (String,Maybe String) -> Html
cgiValueInput (key,Just value) = HE.input `HE.withAttrs` [HA.type_hidden,
                                                          HA.name key,
                                                          HA.value value]
cgiValueInput (key,Nothing) = HE.input `HE.withAttrs` [HA.type_hidden,
                                                       HA.name key]
