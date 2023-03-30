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

import           Wilde.Utils.TextHtmlUtils

import           Wilde.WildeUi.WildeStyles
import           Wilde.WildeUi.WildeStyle
import           Wilde.WildeUi.UiPrimitives

import           Wilde.Render.StyleForHtml
import           Wilde.Render.AbstractTableToHtml


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Renders a page title.
renderPageTitle :: WildeTitle
                   -> Html
renderPageTitle title =
    withclasses classes $
    HE.div (HE.str titleString)
  where
    classes       = titleClasses ++ systemClasses
    systemClasses = Wilde.WildeUi.WildeStyles.pageTitleClasses :: [ClassName]
    titleString   = wildeStyled title :: Title
    titleStyle    = wildeStyle  title :: WildeStyle
    titleClasses  = getClasses titleStyle :: [ClassName]


-- | Renders a page.
renderPage :: [URL]          -- ^ CSS files
           -> WildeStyle     -- ^ page=body style
           -> WildeTitle
           -> [AnyCOMPONENT] -- ^ body
           -> HD.Document
renderPage cssFiles styleForPage title components =
  renderPageHtml cssFiles styleForPage title bodyContents
  where
    bodyContents = HE.seq $ srvcTitle : compsHtml :: Html
    srvcTitle    = renderPageTitle title :: Html
    compsHtml    = map renderTopLevelComponent components :: [Html]

-- | Renders a page given simple Html.
renderPageHtml :: [URL]          -- ^ CSS files
               -> WildeStyle     -- ^ page=body style
               -> WildeTitle     -- ^ Page and service title
               -> Html           -- ^ body
               -> HD.Document
renderPageHtml cssFiles styleForPage title body =
  HD.document headContents body (applyStyleToHtml styleForPage)
  where
    headContents = HE.seq [HD.title titleString, hdrCssLink]
    hdrCssLink   = HE.seq $ map cssRefElem cssFiles
    srvcTitle    = renderPageTitle title :: Html
    titleString  = wildeStyled title :: Title

    cssRefElem  :: URL -> Html
    cssRefElem cssFile = HE.link `HE.withAttrs`
                         [HA.rel "stylesheet"
                         ,HA.href cssFile
                         ,HA.type_ "text/css"]

-- | Renders a component with a given title and content.
renderTopLevelComponent :: AnyCOMPONENT -> Html
renderTopLevelComponent component = addStyleTo unstyled
  where
    addStyleTo :: Html -> Html
    addStyleTo  = withclasses pageTopLevelComponentClasses

    unstyled :: Html
    unstyled  = HE.div $ componentHtml component
