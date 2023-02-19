{-# LANGUAGE TypeSynonymInstances #-}

-- | Functionallity for rendering the
-- types of a Wilde application related to the User Interface
-- as HTML.
module Wilde.Render.RenderAsHtml
       (
         renderComponent,
         renderComponentTitle,

         renderTable,

         renderServiceTitle,
         renderPage,
         renderPageHtml,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Render.Html.Types
import qualified Wilde.Render.Html.Attribute as HA
import qualified Wilde.Render.Html.Element as HE
import qualified Wilde.Render.Html.Document as HD

import Wilde.Utils.TextHtmlUtils

import Wilde.Media.WildeStyle
import Wilde.Media.WildeStyleType
import Wilde.Media.WildeMedia

import Wilde.Render.StyleForHtml

import Wilde.Render.AbstractTableToHtml


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Renders a component with a given title and content.
renderComponent :: Maybe StyledTitle -> Html -> Html
renderComponent Nothing html = HE.center html
renderComponent (Just title) html = HE.seq [HE.center (renderComponentTitle title), html]

-- | Renders a component title.
renderComponentTitle :: StyledTitle -- ^ Component title
                     -> Html
renderComponentTitle title =
    withclasses classes $ HE.div (HE.str titleString)
  where
    classes       = titleClasses ++ systemClasses
    systemClasses = Wilde.Media.WildeStyle.componentTitleClasses :: [ClassName]
    titleString   = wildeStyled title :: Title
    titleStyle    = wildeStyle  title :: WildeStyle
    titleClasses  = getClasses titleStyle :: [ClassName]

-- | Renders a page title.
renderServiceTitle :: StyledTitle
                   -> Html
renderServiceTitle title =
    withclasses classes $
    HE.div (HE.str titleString)
  where
    classes       = titleClasses ++ systemClasses
    systemClasses = Wilde.Media.WildeStyle.pageTitleClasses :: [ClassName]
    titleString   = wildeStyled title :: Title
    titleStyle    = wildeStyle  title :: WildeStyle
    titleClasses  = getClasses titleStyle :: [ClassName]


-- | Renders a page.
renderPage :: Maybe String -- CSS file
           -> StyledTitle
           -> [AnyCOMPONENT] -- ^ body
           -> HD.Document
renderPage mbCssFilePath title components =
    let headContents = HE.seq [HD.title titleString, hdrCssLink]
        hdrCssLink   = maybe HE.empty cssRefElem mbCssFilePath
        compsHtml    = map (renderComponent Nothing . componentHtml) components :: [Html]
        srvcTitle    = renderServiceTitle title :: Html
        bodyContents = HE.seq $ srvcTitle : compsHtml :: Html
    in  HD.document headContents bodyContents (applyStyleToHtml styleForPage)
  where
    cssRefElem   :: String -> Html
    cssRefElem cssFile = HE.link `HE.withAttrs`
                         [HA.rel "stylesheet"
                         ,HA.href cssFile
                         ,HA.custom "type" "text/css"]

    styleForPage  = WildeStyle [pageClass]
    titleString   = wildeStyled title :: Title
    titleStyle    = wildeStyle  title :: WildeStyle
    titleClasses  = getClasses titleStyle :: [ClassName]

instance COMPONENT Html where
  componentHtml = id

-- | Renders a page given simple Html.
renderPageHtml :: Maybe String -- ^ CSS file
               -> StyledTitle -- ^ Page and service title
               -> Html -- ^ body
               -> HD.Document
renderPageHtml mbCssFilePath title body =
  renderPage mbCssFilePath title [AnyCOMPONENT body]
