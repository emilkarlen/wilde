-- | Construction of Wilde Tables

{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.WildeUi.WildeTables
(
  CellType(..),
  dataCellType,
  colHeaderType,
  rowHeaderType,

  wildeCellFromSVALUE,

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
  headerValueTable,
  conWildeHeaderRowTable,
  conWildeHeaderRowTable2,

  conStandardTable,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.GenericUi.AbstractTable as AT
import           Wilde.GenericUi.Style

import           Wilde.WildeUi.WildeValue
import qualified Wilde.WildeUi.WildeStyle as WS

import           Wilde.WildeUi.StdValueTypes
import           Wilde.WildeUi.UiPrimitives
import           Wilde.WildeUi.WildeTable


-------------------------------------------------------------------------------
-- - cell -
-------------------------------------------------------------------------------


wildeCellFromSVALUE :: SVALUE a => CellType -> Span -> a -> WildeCell
wildeCellFromSVALUE cellType span svalue =
  conCell (valueStyle svalue) cellType span (AnyVALUE svalue)

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


-- | Produces a 'WildeTable' where each row is (Header,Value).
headerValueTable
  :: forall a.
     (a -> AnySVALUE)        -- ^ get header
  -> (a -> AnySVALUE)        -- ^ get value
  -> WildeStyle              -- ^ row style
  -> (WildeStyle,WildeStyle) -- ^ (header style, value style)
  -> [a]
  -> WildeTable
headerValueTable renderHeader renderValue rowStyle (headerStyle,valueStyle) rows =
  conTable neutral Nothing Nothing $
    conBodyRowGroup2 rowStyle tableRows
  where
    tableRows  :: [[WildeCell]]
    tableRows   = map row rows

    row        :: a -> [WildeCell]
    row r       = [headerCell r, valueCell r]

    headerCell :: a -> WildeCell
    headerCell  = addStyleToSTYLING headerStyle . cellWContents
      where
        cellWContents = wildeCellFromSVALUE AT.rowHeaderType AT.spanSingle . renderHeader

    valueCell  :: a -> WildeCell
    valueCell   = addStyleToSTYLING valueStyle . cellWContents
      where
        cellWContents = wildeCellFromSVALUE AT.dataCellType  AT.spanSingle . renderValue


-------------------------------------------------------------------------------
-- - conWildeHeaderRowTable -
-------------------------------------------------------------------------------


-- | A table where each row is an element in a list, and the first row
-- is headers for the columns.
conWildeHeaderRowTable :: WildeStyle              -- ^ Row Style for body-rows.
                       -> Maybe WildeTitle        -- ^ Title.
                       -> [WildeTitle]            -- ^ Column titles.
                                                  -- length is equal to num columns
                                                  -- = length of each body row.
                       -> Maybe ([ColGroup WildeStyle],
                                 [[WildeCell]])   -- ^ Footer
                       -> [[ElementWithStyle]]    -- ^ Body rows.
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
  -> Maybe WildeTitle         -- ^ Title.
  -> [WildeTitle]             -- ^ Column titles.
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

conHead :: Maybe WildeTitle -> [WildeTitle] -> Maybe WildeRowGroup
conHead mbTitle columnTitles =
  Just $ conRowGroup neutral [] rows
  -- Just $ conRowGroup neutral (repeat (ColGroup 1 WS.multiColumnTitle)) rows
  where
    rows = maybe [] titleRow mbTitle
           ++
           [conRow neutral $ map mkColTitle columnTitles]

    mkColTitle :: WildeTitle -> WildeCell
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
  -> [[WildeCell]]        -- ^ Rows
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
conStandardTable :: Maybe WildeTitle
                 -> [WildeTitle]
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
  :: Maybe WildeTitle
  -> [WildeTitle]
  -> ([[WildeCell]],[[WildeCell]])
  -- ^ (footer rows, body rows)
  -> WildeTable
conStandardTable2 mbTitle columnTitles (footRows,bodyRows) =
  conWildeHeaderRowTable2 WS.multiRow mbTitle columnTitles mbFoot bodyRows
  where
    mbFoot = if null footRows
             then Nothing
             else Just ([],footRows)
