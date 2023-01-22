-- | Experimenting with convenience functions for constructing tables.

-- stilarna funkar inget vidare. får kolla det sen.
module Wilde.WildeUi.TableUtils
       (
         cellStdEmpty,
         cellStd,
         cellSpaned,
         cellStyled,

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


cellSpaned :: VALUE a => Span -> a -> WildeStyledCell
cellSpaned span a = conCell neutral span (AnyVALUE a)

-- | Creates a styled cell.
-- The cell style is the sum of the mandatory style and the values style.
cellStyled :: SVALUE a =>
              WildeStyle -- ^ Mandatory style.
           -> a          -- ^ Value.
           -> WildeStyledCell
cellStyled mandatoryStyle a =
    let style = addStyle mandatoryStyle (valueStyle a)
    in  conCell style (1,1) (AnyVALUE a)

cellStd :: VALUE a => a -> WildeStyledCell
cellStd x = conCell neutral (1,1) $ AnyVALUE x

cellStdEmpty :: WildeStyledCell
cellStdEmpty = conCell neutral (1,1) $ empty


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
    rowsEWS  :: [[ElementWithStyle]]
    rowsEWS   = map (map SeValue) rowsASV
    rowsASV  :: [[AnySVALUE]]
    rowsASV   = map row rows
    row r     = [renderHeader r,renderValue r]


-------------------------------------------------------------------------------
-- - conWildeHeaderRowTable -
-------------------------------------------------------------------------------


-- | A table where each row is an element in a list, and the first row
-- is headers for the columns.
conWildeHeaderRowTable :: WildeStyle                  -- ^ Row Style for body-rows.
                       -> Maybe StyledTitle        -- ^ Title.
                       -> [StyledTitle]            -- ^ Column-info.
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
             (conBodyRowGroupEvenOdd bodyRowStyle (map wildeStyle columnTitles) bodyData)

conFoot :: (Maybe ([ColGroup WildeStyle],[[WildeStyledCell]])) -> Maybe WildeRowGroup
conFoot Nothing = Nothing
conFoot (Just (colGroups,cells)) = Just $
                                   conRowGroup WS.sumStyle [] $
                                   map (conRow neutral) cells

conHead :: Maybe StyledTitle -> [StyledTitle] -> Maybe WildeRowGroup
conHead mbTitle columnTitles =
  Just $ conRowGroup neutral [] rows
  -- Just $ conRowGroup neutral (repeat (ColGroup 1 WS.multiColumnTitle)) rows
  where
    rows = (maybe [] titleRow mbTitle)
           ++
           [conRow neutral $ map mkColTitle columnTitles]

    mkColTitle :: StyledTitle -> WildeStyledCell
    mkColTitle styledTitle = conCell theStyle (1,1) stringValue
      where
        theStyle    = addStyle WS.attributeTitle $ wildeStyle styledTitle
        stringValue = AnyVALUE (UnquotedStringValue titleString)
        titleString = wildeStyled styledTitle

    titleRow styledTitle = [conRow neutral [cell]]
      where
        cell  = conCell theStyle (length columnTitles,1) $
                AnyVALUE $
                UnquotedStringValue titleString
        titleString = wildeStyled styledTitle
        theStyle    = addStyle WS.multiColumnTitle $ wildeStyle styledTitle


conBodyRowGroup :: WildeStyle           -- ^ The style of each row.
                -> [WildeStyle]         -- ^ The style for each column.
                                        -- Length is equal to the
                -> [[ElementWithStyle]] -- ^ Cells: rows containing a cell for each
                                        -- column.
                -> WildeRowGroup
conBodyRowGroup rowStyle columnStyles rows =
  conRowGroup neutral (map (ColGroup 1) columnStyles) $ map conRow' rows
    where
      conRow' :: [ElementWithStyle] -> StyledRow WildeStyle AnyVALUE
      conRow' cols = conRow rowStyle $ map elementWithStyleToCell cols

conBodyRowGroupEvenOdd :: WildeStyle           -- ^ The style of each row.
                       -> [WildeStyle]         -- ^ The style for each column.
                                               -- Length is equal to the
                       -> [[ElementWithStyle]] -- ^ Cells: rows containing a cell for each
                                               -- column.
                       -> WildeRowGroup
conBodyRowGroupEvenOdd rowStyle columnStyles rows =
  conRowGroup neutral (map (ColGroup 1) columnStyles) $ map conRow' $ zip [0..] rows
    where
      conRow' :: (Int, [ElementWithStyle]) -> StyledRow WildeStyle AnyVALUE
      conRow' (n,cols) = conRow (rowStyle `addStyle` (oddEvenRowStyle n))
                         (map elementWithStyleToCell cols)

      oddEvenRowStyle :: Int -> WildeStyle
      oddEvenRowStyle n = case even n of
        True  -> WS.multiRowEven
        False -> WS.multiRowOdd

-- | Transforms to a cell of span (1,1).
elementWithStyleToCell :: ElementWithStyle -> WildeStyledCell
elementWithStyleToCell ews@(SeHtml _) = conCell neutral        (1,1) $ AnyVALUE ews
elementWithStyleToCell (SeValue x   ) = conCell (valueStyle x) (1,1) $ anySvalue2Value x

-- | Utility method when using 'tableWithFooterRows'.
conStandardTable :: Maybe StyledTitle
                 -> [StyledTitle]
                 -> ([[WildeStyledCell]],[[ElementWithStyle]])
                 -> WildeTable
conStandardTable mbTitle titles (footRows,bodyRows) =
  conWildeHeaderRowTable neutral mbTitle titles mbFoot bodyRows
  where
    mbFoot = if null footRows
             then Nothing
             else (Just ([],footRows))


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
