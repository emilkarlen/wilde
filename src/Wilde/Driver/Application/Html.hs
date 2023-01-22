-- | Tools for application drivers that implement HTML based applications.
module Wilde.Driver.Application.Html
       (
         runService_html,
         runService_htmlString,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Map as Map

import Control.Monad.Trans

import qualified Text.Html as THtml

import Wilde.GenericUi.Components

import           Wilde.Media.WildeMedia
import           Wilde.Media.WildeStyle (errorStyle)
import qualified Wilde.Media.UserInteraction.Output as UiOm

import Wilde.WildeUi.StdValueTypes

import           Wilde.Service.Monad

import           Wilde.Render.ServiceLink
import           Wilde.Render.RenderAsHtml
import qualified Wilde.Driver.Application.Cgi.VariableNames as VariableNames

import qualified Wilde.Application.Service.PopUp as PopUp

import Wilde.Application.ApplicationConfiguration
import Wilde.Application.Service.Service


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


runService_html :: Maybe String
                -> Translations
                -> ServiceEnvironment
                -> Service
                -> IO THtml.Html
runService_html mbCssFile tr env service =
  do
    serviceResult <- liftIO $ runService env service
    let wildePage = renderServiceResult tr env serviceResult
    pure $ renderPageAsHtml wildePage
  where
    renderPageAsHtml :: (StyledTitle,[AnyCOMPONENT]) -> THtml.Html
    renderPageAsHtml (title,components) = renderPage mbCssFile title components

-- | A variant of 'runService_html' that also renders the HTML as a string.
--
-- Renders \"pretty\" if 'VariableNames.pretty' is found in the input media.
runService_htmlString :: Maybe String
                      -> Translations
                      -> ServiceEnvironment
                      -> Service
                      -> IO String
runService_htmlString mbCssFile tr env service =
    do
      html <- runService_html mbCssFile tr env service
      pure $ renderHtmlAsString html
  where
    renderHtmlAsString :: THtml.Html -> String
    renderHtmlAsString = if VariableNames.pretty `Map.member` (envMedia env)
                         then THtml.prettyHtml
                         else THtml.renderHtml

renderServiceResult :: Translations
                    -> ServiceEnvironment
                    -> Either ServiceError ServiceOkResult
                    -> (StyledTitle,[AnyCOMPONENT])
renderServiceResult tr env serviceResult =
    either outputError (processOkResult outputPage outputPopUp) serviceResult
  where
    outputError :: ServiceError -> (StyledTitle,[AnyCOMPONENT])
    outputError err = (withWildeStyle errorStyle (trErorPageTitle tr),
                       [anyValueComponent (UnquotedStringValue (show err))])

    outputPage :: ServicePage -> (StyledTitle,[AnyCOMPONENT])
    outputPage (pageTitle,anyComponents) = (pageTitle, anyComponents)

    outputPopUp :: ServicePopUp -> (StyledTitle,[AnyCOMPONENT])
    outputPopUp = processPopUpOkResult outputAskIfContinue outputInformation

    outputAskIfContinue :: AskIfContinuePopUp -> (StyledTitle,[AnyCOMPONENT])
    outputAskIfContinue popup = (withNeutralWildeStyle (trContinueQuestion tr),
                                 components)
        where
          components   = [msgComponent,btnComponent]
          msgComponent = anyValueComponent $ UnquotedStringValue msg
          btnComponent = buttonLink target PopUp.Yes
          target       = askIfContinueContinuation popup
          msg          = askIfContinueMessage popup

    outputInformation :: InformationPopUp -> (StyledTitle,[AnyCOMPONENT])
    outputInformation popup = (withNeutralWildeStyle (trIformationDialogTitle tr),
                               components)
        where
          components    = msgComponent : btnComponents
          msgComponent  = anyValueComponent $ UnquotedStringValue msg
          btnComponents = maybe []
                          (\target -> [buttonLink target PopUp.Ok])
                          mbTarget
          mbTarget     = informationContinuation popup
          msg          = informationMessage popup

    buttonLink :: ServiceLink -> PopUp.Button -> AnyCOMPONENT
    buttonLink target buttonType =
      anyValueComponent $
      renderServiceLink_svalue btn target
      where
        btn          = AnySVALUE $ button btnText
        btnText      = btnTexter buttonType
        btnTexter    = trButtonTexter . UiOm.outTranslations . envOutputing $ env
