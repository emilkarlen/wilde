module Wilde.Render.AbstractTableToHtml
       (
         Wilde.Render.AbstractTableToHtml.renderTable,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Maybe

import           Wilde.Render.Html.Types
import qualified Wilde.Render.Html.Element as HE
import qualified Wilde.Render.Html.Attribute as HA

import Wilde.GenericUi.AbstractTable
import Wilde.Render.StyleForHtml ( STYLE_FOR_HTML(..) )


-------------------------------------------------------------------------------
-- - renderTable -
-------------------------------------------------------------------------------


-- | Renders a 'StyledTable' as HTML.
--
-- Styles of 'ColGroup's are applied to the cells.
renderTable :: (HTML a,STYLE_FOR_HTML s)
            => StyledTable s a
            -> Html
renderTable = tableMapSimple applyStyleToHtml
              renderTableContents renderRowGroupContents
              renderRowContents renderCellContents . applyColGroupsToCells

renderTableContents :: (Maybe Html,Maybe Html,Html) -> Html
renderTableContents (mbHead,mbFoot,body) = HE.table $ HE.seq [mb mbHead,mb mbFoot,body]
    where
      mb = Data.Maybe.fromMaybe HE.empty


renderRowGroupContents :: STYLE_FOR_HTML style
                       => RowGroupType
                       -> ([ColGroup style],[Html]) -- ^ Rows
                       -> Html
renderRowGroupContents Head (_,htmls) = HE.thead $ HE.seq htmls
renderRowGroupContents Foot (_,htmls) = HE.tfoot $ HE.seq htmls
renderRowGroupContents Body (_,htmls) = HE.tbody $ HE.seq htmls


renderRowContents :: RowGroupType
                  -> [Html] -- ^ Cells
                  -> Html
renderRowContents _ = HE.tr . HE.seq


renderCellContents :: HTML a => RowGroupType -> Cell a -> Html
renderCellContents rgt (Cell celltype cellspan contents) =
  tablecell celltype (toHtml contents) `HE.withAttrs` spans cellspan
  where
    spans :: (Int,Int) -> [HtmlAttr]
    spans (x,y) = span HA.colspan x <> span HA.rowspan y

    span :: (Int -> HtmlAttr) -> Int -> [HtmlAttr]
    span _      1 = []
    span mkAttr n = [mkAttr n]

thdDir :: HeaderDirection -> HtmlAttr
thdDir RowDirection    = HA.scopeRow
thdDir ColumnDirection = HA.scopeCol

tablecell :: CellType -> Html -> Html
tablecell DataCell contents = HE.td contents
tablecell (HeaderCell headerDir) contents = HE.th contents `HE.withAttrs` [thdDir headerDir]
