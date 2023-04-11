module Wilde.ApplicationConstruction.UserInteraction.Output.FormComponent
(
  getFormComponent,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Render.Html.Types as HT
import qualified Wilde.Render.Html.Attribute as HA
import qualified Wilde.Render.Html.Element as HE

import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.UserInteraction as UI
import           Wilde.Media.UserInteraction.Output (PopUpButtonTexter)
import           Wilde.Media.ElementSet (ElementSet, ElementKey, Element, elementKeyRender)

import qualified Wilde.GenericUi.Widget as Widget
import qualified Wilde.WildeUi.WildeStyles as WS
import qualified Wilde.WildeUi.StdValueTypes as StdVT
import qualified Wilde.WildeUi.WildeTable as WT
import qualified Wilde.WildeUi.WildeTables as WTs
import           Wilde.WildeUi.LayoutComponents as LayoutComponents

import qualified Wilde.Render.Cgi.ElementSetIo as ElementSetIo
import qualified Wilde.Render.AbstractTableToHtml

import qualified Wilde.Application.Service.PopUp as PopUp

import qualified Wilde.ApplicationConstruction.Presentation.DataAndButtonsComponent as DataAndButtons


-------------------------------------------------------------------------------
-- - FormBlock -
-------------------------------------------------------------------------------


newtype FormBlockComponent = FormBlockComponent UI.FormBlock

instance COMPONENT FormBlockComponent where
  componentHtml (FormBlockComponent (UI.FormBlock interaction metaValues style)) =
    HE.seq verticalBlocks
    where
      verticalBlocks :: [HE.Html]
      verticalBlocks  = [metaValuesHtml
                        ,Wilde.Render.AbstractTableToHtml.renderTable styledTable]
      styledTable     = addStyleToSTYLING (WS.userInteractionTable <> style) wildeTable
      metaValuesHtml  = HE.seq $ map metaValueInput metaValues
      wildeTable     :: WT.WildeTable
      wildeTable      = WTs.headerValueTable
                        renderHeader
                        renderValue
                        WS.weAttribute WS.tableColumnStylesInputOne
                        interaction
      renderHeader    = either
                        (withNeutralStyleAny . fst)
                        (withNeutralStyleAny . StdVT.UnquotedStringValue . fst)
      renderValue     = either
                        (withNeutralStyleAny . snd)
                        snd


-------------------------------------------------------------------------------
-- - formComponent -
-------------------------------------------------------------------------------


getFormComponent :: UI.Form
                 -> UiO.Monad AnyCOMPONENT
getFormComponent form = do
  custEnv             <- UiO.getCustomEnvironment
  buttonTexter        <- UiO.getEnvs UiO.envButtonTexter
  pure $ AnyCOMPONENT $ FormComponent (custEnv, buttonTexter, form)

newtype FormComponent = FormComponent (ElementSet, PopUpButtonTexter, UI.Form)

instance COMPONENT FormComponent where
  componentHtml (FormComponent (custEnv, buttonTexter, form)) =
    htmlForm action Get htmlFormContents
    where

      action           :: Maybe HT.URL
      action            = UI.formAction form

      custEnvCgiVals    = ElementSetIo.customEnvironmentSetToCgiValues custEnv
      custEnvHtml       = HE.seq $ map cgiValueInput custEnvCgiVals
      formMetasHtml     = HE.seq $ map metaValueInput (UI.formMetaValues form)
      allMetasHtml      = HE.seq [formMetasHtml,custEnvHtml]

      htmlFormContents :: [AnyCOMPONENT]
      htmlFormContents  = [ AnyCOMPONENT $ HtmlOnly allMetasHtml
                           , dataAndButtons (UI.formBlocks form) buttonTexter
                         ]
      dataAndButtons :: [UI.FormBlock] -> PopUpButtonTexter -> AnyCOMPONENT
      dataAndButtons formBlocks buttonTexter =
        DataAndButtons.new formBlocksC (buttons buttonTexter)
        where
          formBlocksC :: AnyCOMPONENT
          formBlocksC  = verticalComponents $ map FormBlockComponent formBlocks

      buttons :: PopUpButtonTexter -> [AnySVALUE]
      buttons buttonTexter =
        [ formButton Reset  (buttonTexter PopUp.Reset)
        , formButton Submit (buttonTexter PopUp.Ok)
        ]

htmlForm :: Maybe HT.URL -> FormMethod -> [AnyCOMPONENT] -> HT.Html
htmlForm action method contents = HE.form contentHtml `HE.withAttrs` formAttrs
  where
    contentHtml    = HE.seq $ map componentHtml contents
    attrListAction = maybe [] (\actn -> [HA.action actn]) action
    formAttrs      = HA.method (show method) : attrListAction

data FormMethod = Get | Post

instance Show FormMethod where
  show Get  = "get"
  show Post = "post"


-------------------------------------------------------------------------------
-- - FormButtons -
-------------------------------------------------------------------------------


formButton :: FormButtonType -> String -> AnySVALUE
formButton buttonType label = AnySVALUE $ withNeutralStyleAny $ FormButton buttonType label

data FormButtonType
  = Submit
  | Reset

-- | A form button - type and label
data FormButton = FormButton FormButtonType String

instance VALUE FormButton where
  valueHtml (FormButton Submit label) = HE.submit label
  valueHtml (FormButton Reset  label) = HE.reset  label


-------------------------------------------------------------------------------
-- - Widget -
-------------------------------------------------------------------------------


-- | Constructs a Html "input" tag with type and key.
inputWithTypeAndKey :: String -- ^ input type (text, checkbox, ...)
                    -> ElementKey
                    -> HT.Html
inputWithTypeAndKey inputType key = withKey key $ HE.input `HE.withAttrs` [HA.type_ inputType]

-- | Adds a key to the given Html.
withKey :: ElementKey -> HT.Html -> HT.Html
withKey key html = html `HE.withAttrs` [HA.name (elementKeyRender key)]


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Label -
-------------------------------------------------------------------------------


instance VALUE Widget.Label where
    valueHtml label = HE.str $ Widget.labelString label


-------------------------------------------------------------------------------
-- - Label -
-------------------------------------------------------------------------------

-- | A HTML "input" element for a Meta Element.
metaValueInput :: Element -> HT.Html
metaValueInput (elementKey,value) =
  HE.input `HE.withAttrs` [HA.type_hidden,
                           HA.name (elementKeyRender elementKey),
                           HA.value value]

-- | A HTML "input" element for a CGI value.
cgiValueInput :: (String,Maybe String) -> HT.Html
cgiValueInput (key,Just value) = HE.input `HE.withAttrs` [HA.type_hidden,
                                                          HA.name key,
                                                          HA.value value]
cgiValueInput (key,Nothing) = HE.input `HE.withAttrs` [HA.type_hidden,
                                                       HA.name key]
