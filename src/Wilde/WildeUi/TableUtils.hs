-- | Experimenting with convenience functions for constructing tables.

-- stilarna funkar inget vidare. får kolla det sen.
module Wilde.WildeUi.TableUtils
       (
         cellStdEmpty,
         dataCellStdEmpty,

         cellStd,
         dataCellStd,

         cellSpaned,
         dataCellSpaned,

         cellStyled,
         dataCellStyled,

         wildeHeaderValueTable,
         conWildeHeaderRowTable,

         tableWithFooterRows,
         tableWithFooterRowsM,
         conStandardTable,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.GenericUi.AbstractTable
import Wilde.GenericUi.Style

import Wilde.Media.WildeValue

import qualified Wilde.Media.WildeStyle as WS
import Wilde.Media.WildeMedia

import Wilde.WildeUi.StdValueTypes


-------------------------------------------------------------------------------
-- - cell -
-------------------------------------------------------------------------------


cellSpaned :: VALUE a => CellType -> Span -> a -> WildeStyledCell
cellSpaned type_ span a = conCell neutral type_ span (AnyVALUE a)

dataCellSpaned :: VALUE a => Span -> a -> WildeStyledCell
dataCellSpaned = cellSpaned DataCell

-- | Creates a styled cell.
-- The cell style is the sum of the mandatory style and the values style.
cellStyled :: SVALUE a
           => CellType
           -> WildeStyle -- ^ Mandatory style.
           -> a          -- ^ Value.
           -> WildeStyledCell
cellStyled type_ mandatoryStyle a =
    let style = addStyle mandatoryStyle (valueStyle a)
    in  conCell style type_ (1,1) (AnyVALUE a)

dataCellStyled
  :: SVALUE a
  => WildeStyle -- ^ Mandatory style.
  -> a          -- ^ Value.
  -> WildeStyledCell
dataCellStyled = cellStyled DataCell

cellStd :: VALUE a => CellType -> a -> WildeStyledCell
cellStd type_ x = conCell neutral type_ (1,1) $ AnyVALUE x

dataCellStd :: VALUE a => a -> WildeStyledCell
dataCellStd = cellStd DataCell

cellStdEmpty :: CellType -> WildeStyledCell
cellStdEmpty type_ = conCell neutral type_ (1,1) $ empty

dataCellStdEmpty :: WildeStyledCell
dataCellStdEmpty = cellStdEmpty DataCell


-------------------------------------------------------------------------------
-- - wildeHeaderValueTable -
-------------------------------------------------------------------------------


-- | Produces a 'WildeTable' where each row is (Header,Value).

wildeHeaderValueTable :: (a -> AnySVALUE)
                      -> (a -> AnySVALUE)
                      -> WildeStyle              -- ^ Row style
                      -> (WildeStyle,WildeStyle) -- ^ (Header style, Value style)
                      -> [a]
                      -> WildeTable
wildeHeaderValueTable renderHeader renderValue rowStyle (headerStyle,valueStyle) rows =
  conTable neutral Nothing Nothing (conBodyRowGroup
                                    rowStyle
                                    [headerStyle,valueStyle]
                                    rowsEWS)
  where
    rowsEWS  :: [[(CellType, ElementWithStyle)]]
    rowsEWS   = map row rows
    row r     = [ (rowHeaderType, SeValue $ renderHeader r)
                , (dataCellType, SeValue $ renderValue r)
                ]


-------------------------------------------------------------------------------
-- - conWildeHeaderRowTable -
-------------------------------------------------------------------------------


-- | A table where each row is an element in a list, and the first row
-- is headers for the columns.
conWildeHeaderRowTable :: WildeStyle                  -- ^ Row Style for body-rows.
                       -> Maybe StyledTitle        -- ^ Title.
                       -> [StyledTitle]            -- ^ Column titles.
                                                      -- length is equal to num columns
                                                      -- = length of each body row.
                       -> Maybe ([ColGroup WildeStyle],
                                 [[WildeStyledCell]]) -- ^ Footer
                       -> [[ElementWithStyle]]        -- ^ Body rows.
                                                      -- length is equal to num columns.
                                                      -- Each cell spans (1,1).
                       -> WildeTable
conWildeHeaderRowTable bodyRowStyle mbTitle columnTitles mbFoot bodyData =
  conTable neutral
           (conHead mbTitle columnTitles)
           (conFoot mbFoot)
           (conBodyRowGroup bodyRowStyle (map wildeStyle columnTitles) bodyData')
  where
    bodyData' :: [[(CellType,ElementWithStyle)]]
    bodyData' = map (map setDataCellType) bodyData

    setDataCellType :: ElementWithStyle -> (CellType,ElementWithStyle)
    setDataCellType x = (DataCell, x)

conFoot :: Maybe ([ColGroup WildeStyle],[[WildeStyledCell]]) -> Maybe WildeRowGroup
conFoot Nothing = Nothing
conFoot (Just (colGroups,cells)) = Just $
                                   conRowGroup WS.sumStyle [] $
                                   map (conRow neutral) cells

conHead :: Maybe StyledTitle -> [StyledTitle] -> Maybe WildeRowGroup
conHead mbTitle columnTitles =
  Just $ conRowGroup neutral [] rows
  -- Just $ conRowGroup neutral (repeat (ColGroup 1 WS.multiColumnTitle)) rows
  where
    rows = maybe [] titleRow mbTitle
           ++
           [conRow neutral $ map mkColTitle columnTitles]

    mkColTitle :: StyledTitle -> WildeStyledCell
    mkColTitle styledTitle = conCell theStyle colHeaderType (1,1) stringValue
      where
        theStyle    = addStyle WS.attributeTitle $ wildeStyle styledTitle
        stringValue = AnyVALUE (UnquotedStringValue titleString)
        titleString = wildeStyled styledTitle

    titleRow styledTitle = [conRow neutral [cell]]
      where
        cell  = conCell theStyle colHeaderType (length columnTitles,1) $
                AnyVALUE $
                UnquotedStringValue titleString
        titleString = wildeStyled styledTitle
        theStyle    = addStyle WS.multiColumnTitle $ wildeStyle styledTitle

conBodyRowGroup :: WildeStyle           -- ^ The style of each row.
                -> [WildeStyle]         -- ^ The style for each column.
                                        -- Length is equal to the number of cells
                -> [[(CellType, ElementWithStyle)]] -- ^ Cells: rows containing a cell for each
                                        -- column.
                -> WildeRowGroup
conBodyRowGroup rowStyle columnStyles rows =
  conRowGroup neutral (map (ColGroup 1) columnStyles) $ map conRow' rows
    where
      conRow' :: [(CellType, ElementWithStyle)] -> StyledRow WildeStyle AnyVALUE
      conRow' cols = conRow rowStyle $ map (uncurry elementWithStyleToCell) cols

-- | Transforms to a cell of span (1,1).
elementWithStyleToCell :: CellType -> ElementWithStyle -> WildeStyledCell
elementWithStyleToCell type_ ews@(SeHtml _) = conCell neutral        type_ (1,1) $ AnyVALUE ews
elementWithStyleToCell type_ (SeValue x   ) = conCell (valueStyle x) type_ (1,1) $ anySvalue2Value x

-- | Utility method when using 'tableWithFooterRows'.
conStandardTable :: Maybe StyledTitle
                 -> [StyledTitle]
                 -> ([[WildeStyledCell]],[[ElementWithStyle]])
                 -> WildeTable
conStandardTable mbTitle titles (footRows,bodyRows) =
  conWildeHeaderRowTable WS.multiRow mbTitle titles mbFoot bodyRows
  where
    mbFoot = if null footRows
             then Nothing
             else Just ([],footRows)

-- | s is the
-- type of the state that accumulates the data from which the footer data is
-- generated.  s is the accumulation of body data rows.
tableWithFooterRows :: (s -> record -> s)           -- ^ Footer data accumulator
                    -> s                            -- ^ Footer accumulated data start value.
                    -> (s -> [[WildeStyledCell]])   -- ^ Constructs footer table
                                                    -- list rows.
                    -> (record -> [ElementWithStyle])  -- ^ Gets a row of
                                                    -- table list body
                                                    -- data.
                    -> (([[WildeStyledCell]],[[ElementWithStyle]]) -> WildeTable)
                    -> [record]
                    -> WildeTable
tableWithFooterRows footerDataAccumulator s0 footerDataConstructor
                  bodyDataRowGetter
                  conTable
                  records =
  conTable (footerDataConstructor sn,reverse b)
  where
      conRows (bodyRows,footerState) record =
            let nextBodyRow     = bodyDataRowGetter record
                nextFooterState = footerDataAccumulator footerState record
            in  (nextBodyRow : bodyRows,nextFooterState)
      (b,sn) = foldl conRows ([],s0) records

-- | s is the
-- type of the state that accumulates the data from which the footer data is
-- generated.  s is the accumulation of body data rows.
tableWithFooterRowsM :: Monad m
                    => (s -> record -> s)           -- ^ Footer data accumulator
                    -> s                            -- ^ Footer accumulated data start value.
                    -> (s -> [[WildeStyledCell]])   -- ^ Constructs footer table
                                                    -- list rows.
                    -> (record -> m [ElementWithStyle])  -- ^ Gets a row of
                                                    -- table list body
                                                    -- data.
                    -> (([[WildeStyledCell]],[[ElementWithStyle]]) -> WildeTable)
                    -> [record]
                    -> m WildeTable
tableWithFooterRowsM footerDataAccumulator s0 footerDataConstructor
                  bodyDataRowGetter
                  conTable
                  records =
  do
    recordsElementWithStyleList <- mapM bodyDataRowGetter records
    pure $ conTable (footerDataConstructor sN,recordsElementWithStyleList)
  where
    sN = foldl footerDataAccumulator s0 records
