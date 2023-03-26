-- | Tools for application drivers that implement HTML based applications.
module Wilde.Driver.Application.Web.RunService
       (
         HtmlAsString,

         runService_html,
         runService_htmlString,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Map as Map

import Control.Monad.Trans

import           Wilde.Render.Html.Types
import qualified Wilde.Render.Html.Render as HR

import           Wilde.GenericUi.Components

import           Wilde.WildeUi.WildeStyle (errorStyle)
import           Wilde.WildeUi.StdValueTypes
import           Wilde.WildeUi.UiPrimitives (WildeTitle)

import qualified Wilde.Media.UserInteraction.Output as UiOm

import           Wilde.Service.Monad

import           Wilde.Render.ServiceLink
import           Wilde.Render.RenderAsHtml

import qualified Wilde.Driver.Application.Cgi.VariableNames as VariableNames
import           Wilde.Driver.Application.Web.Types (HtmlAsString)

import qualified Wilde.Application.Service.PopUp as PopUp

import           Wilde.Application.ApplicationConfiguration
import           Wilde.Application.Service.Service


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


runService_html :: [URL]
                -> Translations
                -> ServiceEnvironment
                -> Service
                -> IO Html
runService_html cssFiles tr env service =
  do
    serviceResult <- liftIO $ runService env service
    let wildePage = renderServiceResult tr env serviceResult
    pure $ renderPageAsHtml wildePage
  where
    renderPageAsHtml :: (WildeTitle,[AnyCOMPONENT]) -> Html
    renderPageAsHtml (title,components) = renderPage cssFiles title components

-- | A variant of 'runService_html' that also renders the HTML as a string.
--
-- Renders \"pretty\" if 'VariableNames.pretty' is found in the input media.
runService_htmlString :: [URL]
                      -> Translations
                      -> ServiceEnvironment
                      -> Service
                      -> IO HtmlAsString
runService_htmlString cssFiles tr env service =
    do
      html <- runService_html cssFiles tr env service
      pure $ renderHtmlAsString html
  where
    renderHtmlAsString :: Html -> String
    renderHtmlAsString = if VariableNames.pretty `Map.member` (envMedia env)
                         then HR.pretty
                         else HR.standard

renderServiceResult :: Translations
                    -> ServiceEnvironment
                    -> Either ServiceError ServiceOkResult
                    -> (WildeTitle,[AnyCOMPONENT])
renderServiceResult tr env serviceResult =
    either outputError (processOkResult outputPage outputPopUp) serviceResult
  where
    outputError :: ServiceError -> (WildeTitle,[AnyCOMPONENT])
    outputError err = (withWildeStyle errorStyle (trErorPageTitle tr),
                       [anyValueComponent (UnquotedStringValue (show err))])

    outputPage :: ServicePage -> (WildeTitle,[AnyCOMPONENT])
    outputPage (pageTitle,anyComponents) = (pageTitle, anyComponents)

    outputPopUp :: ServicePopUp -> (WildeTitle,[AnyCOMPONENT])
    outputPopUp = processPopUpOkResult outputAskIfContinue outputInformation

    outputAskIfContinue :: AskIfContinuePopUp -> (WildeTitle,[AnyCOMPONENT])
    outputAskIfContinue popup = (withNeutralWildeStyle (trContinueQuestion tr),
                                 components)
        where
          components   = [msgComponent,btnComponent]
          msgComponent = anyValueComponent $ UnquotedStringValue msg
          btnComponent = buttonLink target PopUp.Yes
          target       = askIfContinueContinuation popup
          msg          = askIfContinueMessage popup

    outputInformation :: InformationPopUp -> (WildeTitle,[AnyCOMPONENT])
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
