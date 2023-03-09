{-# LANGUAGE FlexibleInstances #-}

-- | Functionallity for rendering the
-- types of a Wilde application related to the User Interface
-- as HTML.
module Wilde.Render.RenderAsHtml
       (
         renderTable,

         renderPageTitle,
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


-- | Renders a page title.
renderPageTitle :: StyledTitle
                   -> Html
renderPageTitle title =
    withclasses classes $
    HE.div (HE.str titleString)
  where
    classes       = titleClasses ++ systemClasses
    systemClasses = Wilde.Media.WildeStyle.pageTitleClasses :: [ClassName]
    titleString   = wildeStyled title :: Title
    titleStyle    = wildeStyle  title :: WildeStyle
    titleClasses  = getClasses titleStyle :: [ClassName]


-- | Renders a page.
renderPage :: [URL] -- ^ CSS files
           -> StyledTitle
           -> [AnyCOMPONENT] -- ^ body
           -> HD.Document
renderPage cssFiles title components =
    let headContents = HE.seq [HD.title titleString, hdrCssLink]
        hdrCssLink   = HE.seq $ map cssRefElem cssFiles
        compsHtml    = map renderTopLevelComponent components :: [Html]
        srvcTitle    = renderPageTitle title :: Html
        bodyContents = HE.seq $ srvcTitle : compsHtml :: Html
    in  HD.document headContents bodyContents (applyStyleToHtml styleForPage)
  where
    cssRefElem   :: URL -> Html
    cssRefElem cssFile = HE.link `HE.withAttrs`
                         [HA.rel "stylesheet"
                         ,HA.href cssFile
                         ,HA.type_ "text/css"]

    styleForPage  = WildeStyle [pageClass]
    titleString   = wildeStyled title :: Title

-- | Renders a page given simple Html.
renderPageHtml :: [URL] -- ^ CSS files
               -> StyledTitle -- ^ Page and service title
               -> Html -- ^ body
               -> HD.Document
renderPageHtml cssFiles title body =
  renderPage cssFiles title [AnyCOMPONENT (HtmlOnly body)]

-- | Renders a component with a given title and content.
renderTopLevelComponent :: AnyCOMPONENT -> Html
renderTopLevelComponent component = addStyleTo unstyled
  where
    addStyleTo :: Html -> Html
    addStyleTo  = withclasses pageTopLevelComponentClasses

    unstyled :: Html
    unstyled  = HE.div $ componentHtml component
