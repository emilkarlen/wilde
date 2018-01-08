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

module Wilde.Render.AbstractTableToHtml
       (
         Wilde.Render.AbstractTableToHtml.renderTable,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Text.Html

import Wilde.Utils.TextHtmlUtils

import Wilde.GenericUi.AbstractTable
import Wilde.Render.StyleForHtml


-------------------------------------------------------------------------------
-- - renderTable -
-------------------------------------------------------------------------------


-- | Renders a 'StyledTable' as HTML.
--
-- Styles of 'ColGroup's are applied to the cells.
renderTable :: (HTML a,STYLE_FOR_HTML s) =>
               StyledTable s a
            -> Html
renderTable = tableMapSimple applyStyleToHtml
              renderTableContents renderRowGroupContents
              renderRowContents renderCellContents . applyColGroupsToCells

renderTableContents :: (Maybe Html,Maybe Html,Html) -> Html
renderTableContents (mbHead,mbFoot,body) = table $ concatHtml [mb mbHead,mb mbFoot,body]
    where
      mb = maybe noHtml id

renderRowGroupContents :: STYLE_FOR_HTML style =>
                          RowGroupType
                       -> ([ColGroup style],[Html]) -- ^ Rows
                       -> Html
renderRowGroupContents Head (_,htmls) = thead $ concatHtml htmls
renderRowGroupContents Foot (_,htmls) = tfoot $ concatHtml htmls
renderRowGroupContents Body (_,htmls) = tbody $ concatHtml htmls


renderRowContents :: RowGroupType
                  -> [Html] -- ^ Cells
                  -> Html
renderRowContents _ = tr . concatHtml

renderCellContents :: HTML a => RowGroupType -> (Span,a) -> Html
renderCellContents rgt (cellspan,c) =
    let tablecell = if rgt == Head
                    then th
                    else td
    in  tablecell (toHtml c) ! spans cellspan
        where
          spans :: (Int,Int) -> [HtmlAttr]
          spans (x,y) = span colspan x ++ span rowspan y

          span :: (Int -> HtmlAttr) -> Int -> [HtmlAttr]
          span _      1 = []
          span mkAttr n = [mkAttr n]
