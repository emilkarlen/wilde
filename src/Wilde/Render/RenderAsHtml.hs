{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

-- | Functionallity for rendering the
-- types of a Wilde application related to the User Interface
-- as HTML.
module Wilde.Render.RenderAsHtml
       (
         renderComponent,
         renderComponentTitle,
       
         Wilde.Render.AbstractTableToHtml.renderTable,
         
         renderServiceTitle,
         renderPage,
         renderPageHtml,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Text.Html  as H hiding (HtmlTable)

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
renderComponent :: Maybe StyledTitle -> H.Html -> H.Html
renderComponent Nothing html = H.center html
renderComponent (Just title) html = H.center (renderComponentTitle title) H.+++ html

-- | Renders a component title.
renderComponentTitle :: StyledTitle -- ^ Component title
                        -> H.Html
renderComponentTitle title =
    withclasses classes $ H.thediv (H.stringToHtml titleString)
  where
    classes       = titleClasses ++ systemClasses
    systemClasses = Wilde.Media.WildeStyle.componentTitleClasses :: [ClassName]
    titleString   = wildeStyled title :: Title         
    titleStyle    = wildeStyle  title :: WildeStyle
    titleClasses  = getClasses titleStyle :: [ClassName]
    
-- | Renders a page title.
renderServiceTitle :: StyledTitle
                   -> H.Html
renderServiceTitle title =
    withclasses classes $
    H.thediv (H.stringToHtml 
              titleString) 
  where
    classes       = titleClasses ++ systemClasses
    systemClasses = Wilde.Media.WildeStyle.pageTitleClasses :: [ClassName]
    titleString   = wildeStyled title :: Title         
    titleStyle    = wildeStyle  title :: WildeStyle
    titleClasses  = getClasses titleStyle :: [ClassName]


-- | Renders a page.
renderPage :: Maybe String
           -> StyledTitle
           -> [AnyCOMPONENT] -> H.Html
renderPage mbCssFilePath title components = 
    let pageHead  = H.header $ H.thetitle (H.stringToHtml titleString) H.+++ hdrCssLink
        hdrCssLink = maybe
                     H.noHtml
                     (\cssFile -> linkelem H.! [H.rel "stylesheet"
                                               ,H.href cssFile
                                               ,H.HtmlAttr "type" "text/css"])
                     mbCssFilePath
        compsHtml = foldl (H.+++) H.noHtml $ map (renderComponent Nothing . componentHtml) components
        pageBody  = (renderServiceTitle title) H.+++ compsHtml
    in  H.thehtml $ pageHead H.+++ (applyStyleToHtml styleForPage (H.body pageBody))
  where
    styleForPage  = WildeStyle [pageClass]
    titleString   = wildeStyled title :: Title         
    titleStyle    = wildeStyle  title :: WildeStyle
    titleClasses  = getClasses titleStyle :: [ClassName]

instance COMPONENT H.Html where
  componentHtml = id

-- | Renders a page given simple Html.
renderPageHtml :: Maybe String
               -> StyledTitle -- ^ Page and service title
               -> H.Html 
               -> H.Html
renderPageHtml mbCssFilePath title html =
  renderPage mbCssFilePath title [AnyCOMPONENT html]
