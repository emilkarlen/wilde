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
