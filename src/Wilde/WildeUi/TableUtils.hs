-- | Experimenting with convenience functions for constructing tables.

-- stilarna funkar inget vidare. får kolla det sen.
module Wilde.WildeUi.TableUtils
       (
         cellStdEmpty,
         dataCellStdEmpty,

         cellStd,
         dataCellStd,
         headerForColCellStd,
         headerForRowCellStd,

         cellSpaned,
         dataCellSpaned,

         cellStyled,
         dataCellStyled,

         cellOfStyling,
         dataCellOfStyling,

         wildeHeaderValueTable,
         conWildeHeaderRowTable,
         conWildeHeaderRowTable2,

         tableWithFooterRows,
         tableWithFooterRowsM,
         conStandardTable,

         FooterRowsSetup(..),
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


cellSpaned :: VALUE a => CellType -> Span -> a -> WildeCell
cellSpaned type_ span a = conCell neutral type_ span (AnyVALUE a)

dataCellSpaned :: VALUE a => Span -> a -> WildeCell
dataCellSpaned = cellSpaned DataCell

-- | Creates a styled cell.
-- The cell style is the sum of the mandatory style and the values style.
cellStyled :: SVALUE a
           => CellType
           -> WildeStyle -- ^ Mandatory style.
           -> a          -- ^ Value.
           -> WildeCell
cellStyled type_ mandatoryStyle a =
    let style = addStyle mandatoryStyle (valueStyle a)
    in  conCell style type_ spanSingle (AnyVALUE a)

cellOfStyling :: SVALUE a
              => CellType
              -> a
              -> WildeCell
cellOfStyling type_ a =
    let style = valueStyle a
    in  conCell style type_ spanSingle (AnyVALUE a)

dataCellOfStyling
  :: SVALUE a
  => a          -- ^ Value.
  -> WildeCell
dataCellOfStyling = cellOfStyling DataCell

dataCellStyled
  :: SVALUE a
  => WildeStyle -- ^ Mandatory style.
  -> a          -- ^ Value.
  -> WildeCell
dataCellStyled = cellStyled DataCell

cellStd :: VALUE a => CellType -> a -> WildeCell
cellStd type_ x = conCell neutral type_ spanSingle $ AnyVALUE x

dataCellStd :: VALUE a => a -> WildeCell
dataCellStd = cellStd DataCell

headerForColCellStd, headerForRowCellStd :: VALUE a => a -> WildeCell
headerForColCellStd = cellStd colHeaderType
headerForRowCellStd = cellStd rowHeaderType

cellStdEmpty :: CellType -> WildeCell
cellStdEmpty type_ = conCell neutral type_ spanSingle empty

dataCellStdEmpty :: WildeCell
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
                                 [[WildeCell]]) -- ^ Footer
                       -> [[ElementWithStyle]]        -- ^ Body rows.
                                                      -- length is equal to num columns.
                                                      -- Each cell spans spanSingle.
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

-- | A table where each row is an element in a list, and the first row
-- is headers for the columns.
conWildeHeaderRowTable2
  :: WildeStyle               -- ^ Row Style for body-rows.
  -> Maybe StyledTitle        -- ^ Title.
  -> [StyledTitle]            -- ^ Column titles.
                              -- length is equal to num columns
                              -- = length of each body row.
  -> Maybe ([ColGroup WildeStyle],[[WildeCell]])
  -- ^ footer rows
  -> [[WildeCell]]
  -- ^ body rows
  -> WildeTable
conWildeHeaderRowTable2 bodyRowStyle mbTitle columnTitles mbFoot bodyRows =
  conTable neutral
           (conHead mbTitle columnTitles)
           (conFoot mbFoot)
           (conBodyRowGroup2 bodyRowStyle bodyRows)

conFoot :: Maybe ([ColGroup WildeStyle],[[WildeCell]]) -> Maybe WildeRowGroup
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

    mkColTitle :: StyledTitle -> WildeCell
    mkColTitle styledTitle = conCell theStyle colHeaderType spanSingle stringValue
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
      conRow' :: [(CellType, ElementWithStyle)] -> WildeRow
      conRow' cols = conRow rowStyle $ map (uncurry elementWithStyleToCell) cols

conBodyRowGroup2
  :: WildeStyle           -- ^ The style of each row.
  -> [[WildeCell]]        -- ^ Cells
  -> WildeRowGroup
conBodyRowGroup2 rowStyle rows =
  -- conRowGroup neutral (map (ColGroup 1) columnStyles) $ map conRow' rows
  conRowGroup neutral [] $ map conRow' rows
    where
      conRow' :: [WildeCell] -> WildeRow
      conRow' cols = conRow rowStyle cols

-- | Transforms to a cell of span spanSingle.
elementWithStyleToCell :: CellType -> ElementWithStyle -> WildeCell
elementWithStyleToCell type_ ews@(SeHtml _) = conCell neutral        type_ spanSingle $ AnyVALUE ews
elementWithStyleToCell type_ (SeValue x   ) = conCell (valueStyle x) type_ spanSingle $ anySvalue2Value x

-- | Utility method when using 'tableWithFooterRows'.
conStandardTable :: Maybe StyledTitle
                 -> [StyledTitle]
                 -> ([[WildeCell]],[[ElementWithStyle]])
                 -- ^ (footer rows, body rows)
                 -> WildeTable
conStandardTable mbTitle columnTitles (footRows,bodyRows) =
  conWildeHeaderRowTable WS.multiRow mbTitle columnTitles mbFoot bodyRows
  where
    mbFoot = if null footRows
             then Nothing
             else Just ([],footRows)

-- | Utility method when using 'tableWithFooterRows'.
conStandardTable2
  :: Maybe StyledTitle
  -> [StyledTitle]
  -> ([[WildeCell]],[[WildeCell]])
  -- ^ (footer rows, body rows)
  -> WildeTable
conStandardTable2 mbTitle columnTitles (footRows,bodyRows) =
  conWildeHeaderRowTable2 WS.multiRow mbTitle columnTitles mbFoot bodyRows
  where
    mbFoot = if null footRows
             then Nothing
             else Just ([],footRows)


data FooterRowsSetup acc record =
  FooterRowsSetup
  {
    frsAdd        :: acc -> record -> acc
  , frsZero       :: acc
  , frsRenderRows :: acc -> [[WildeCell]]
  }

-- | s is the
-- type of the state that accumulates the data from which the footer data is
-- generated.  s is the accumulation of body data rows.
tableWithFooterRows
  :: FooterRowsSetup s record
  -> (record -> [ElementWithStyle])
  -- ^ Gets a row of table list body data.
  -> (([[WildeCell]],[[ElementWithStyle]]) -> WildeTable)
  -> [record]
  -> WildeTable
tableWithFooterRows (FooterRowsSetup footerDataAccumulator s0 footerDataConstructor)
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
tableWithFooterRowsM
  :: Monad m
  => FooterRowsSetup s record
                                  -- list rows.
  -> (record -> m [ElementWithStyle])  -- ^ Gets a row of
                                  -- table list body
                                  -- data.
  -> (([[WildeCell]],[[ElementWithStyle]]) -> WildeTable)
  -> [record]
  -> m WildeTable
tableWithFooterRowsM (FooterRowsSetup footerDataAccumulator s0 footerDataConstructor)
                  bodyDataRowGetter
                  conTable
                  records =
  do
    recordsElementWithStyleList <- mapM bodyDataRowGetter records
    pure $ conTable (footerDataConstructor sN,recordsElementWithStyleList)
  where
    sN = foldl footerDataAccumulator s0 records
