-------------------------------------------------------------------------------
-- | A table parametrizes by the type of contents.  This enables the type to
-- be used for different kinds of tables (HTML, XSL, DocBook, ...).
--
--   This type is relatively primitive - it is very \"pysical\".
-- This means that, probably, you want to work with tables on a higher, more
-- logical, level that suits your specific application.  ('ColGroup' and
-- 'applyColGroups' may help you to do this.)
--
--   The only layout and style customizations are
--  * column and rowspan of table cells
--  * \"classes\" on the table itself, \"rowgroups\" and \"cells\".
--
-- \"classes\" corresponds to CSS classes.  They are just strings.  It is up to
-- each concretization (= concrete content type) to interpret these strings.
--
-- FURTHER DEVELOPMENT
--
--  * Add caption
--
--  * Add table settings (may introduce a type parameter for this).
--
--  * Many body row-groups (as HTML tables may have).
-------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.GenericUi.AbstractTable
       -- (
       --   Span,
       --   RowGroupType,
       --   TableMapFun,
       --   RowGroupMapFun,
       --   RowMapFun,
       --   CellMapFun,

       --   StyledTable,
       --   conTable,

       --   StyledRowGroup,
       --   conRowGroup,

       --   StyledRow,
       --   conRow,

       --   StyledCell,
       --   conCell,
       --   tableMap
       -- )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.GenericUi.Style


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type Span = (Int,Int)

-- | Identifies the types of row groups of a table.
data RowGroupType = Head | Foot | Body
                  deriving Eq

-- | Part of a table that can be styled - attatched with style.

-- type Table s = Styling s (TableContent contentType)

data Table style contentType = Table
    {
      tblHead :: Maybe (StyledRowGroup style contentType),
      tblFoot :: Maybe (StyledRowGroup style contentType),
      tblBody ::        StyledRowGroup style contentType
    }

type StyledTable style contentType = Styling style (Table style contentType)

-- | Constructs a table.
conTable :: style                            -- ^ The style of the table.
         -> Maybe (StyledRowGroup style cc)  -- ^ The head row-group.
         -> Maybe (StyledRowGroup style cc)  -- ^ The foot row-group.
         -> StyledRowGroup style cc          -- ^ The body row-group.
         -> StyledTable style cc
conTable style mbHead mbFoot body =
    Styling style $ Table
               {
                 tblHead = mbHead,
                 tblFoot = mbFoot,
                 tblBody = body
               }

type TableMapFun s rowGroup out
    =  s              -- ^ Style for the RowGroup.
    -> (Maybe rowGroup,Maybe rowGroup,rowGroup)  -- ^ Head, foot, body
    -> out

tableMap :: forall s cc cell row rowGroup out.
            TableMapFun    s             rowGroup out
         -> RowGroupMapFun s         row rowGroup
         -> RowMapFun      s    cell row
         -> CellMapFun     s cc cell
         -> StyledTable    s cc
         -> out
tableMap tableMapFun rowGroupMapFun rowMapFun cellMapFun (Styling s (Table mbHead mbFoot body)) =
    tableMapFun s (mbHead',mbFoot',body')
    where
      procRg :: RowGroupType -> StyledRowGroup s cc -> rowGroup
      procRg rgt rowGroup = rowGroupMap rgt rowGroupMapFun rowMapFun cellMapFun rowGroup

      procMbRg :: RowGroupType -> Maybe (StyledRowGroup s cc) -> Maybe rowGroup
      procMbRg rgt mbRowGroup = fmap (procRg rgt) mbRowGroup

      mbHead' = procMbRg Head mbHead
      mbFoot' = procMbRg Foot mbFoot
      body'   = procRg   Body body

-------------------------------------------------------------------------------
-- | Simple mapping of a table.
--
-- \"Simple\" means:
--
-- * all elements of the table (cell, row, ...) are transformed
--   to elements of the same type (a).
--
-- * Style is applied on the result of the transformation, and style is
--   applied in the same way to all kinds of transformation results, i.e., the
--   result of transforming cells, rows, ...
-------------------------------------------------------------------------------
tableMapSimple :: forall s a cc.
     (s -> a -> a)              -- ^ Applies style.
  -> ((Maybe a,Maybe a,a) -> a) -- ^ Transforms transformed row-groups (table).
  -> (RowGroupType -> ([ColGroup s],[a]) -> a) -- ^ Transforms transformed rows (row-group).
  -> (RowGroupType -> [a] -> a) -- ^ Transforms transformed cells (row).
  -> (RowGroupType -> Cell cc -> a) -- ^ Transforms an unstyled cell.
  -> StyledTable    s cc              -- ^ The table to transform.
  -> a
tableMapSimple applyStyle tableMapFun rowGroupMapFun rowMapFun cellMapFun table =
    tableMap tf rgf rf cf table
    where
      tf s = applyStyle s . tableMapFun
      rgf  = withWildeStyle rowGroupMapFun
      rf   = withWildeStyle rowMapFun
      cf   = withWildeStyle cellMapFun

      withWildeStyle :: (RowGroupType -> x -> a) -> RowGroupType -> s -> x -> a
      withWildeStyle transform rgt style x = applyStyle style $ transform rgt x

-- | A "row group" is a list of rows of a table.
-- Used for the "head", "body" and "footer" groups of rows of a table.
data RowGroup style contentType = RowGroup
    {
      rgColGroups :: [ColGroup style],
      rgRows      :: [StyledRow style contentType]
    }

type StyledRowGroup style contentType = Styling style (RowGroup style contentType)

conRowGroup :: style               -- ^ The style of the row-group.
            -> [ColGroup style]
            -> [StyledRow style cc] -- ^ The rows of the row-group.
            -> StyledRowGroup style cc
conRowGroup style cgs rows = Styling style (RowGroup cgs rows)

type RowGroupMapFun s row out
    =  RowGroupType
    -> s                      -- ^ Style for the RowGroup.
    -> ([ColGroup s],[row])
    -> out

rowGroupMap :: RowGroupType
            -> RowGroupMapFun    s         row rowGroup
            -> RowMapFun         s    cell row
            -> CellMapFun        s cc cell
            -> StyledRowGroup s cc
            -> rowGroup
rowGroupMap rgt rowGroupMapFun rowMapFun cellMapFun (Styling s (RowGroup cgs rows)) =
    let rows' = map (rowMap rgt rowMapFun cellMapFun) rows
    in  rowGroupMapFun rgt s (cgs,rows')

newtype Row style contentType = Row
  {
    rowCells :: [StyledCell style contentType] -- ^ The cells.
  }

type StyledRow style contentType = Styling style (Row style contentType)

-- ^ Conucts a table row.
conRow :: style                 -- ^ The style of the row.
       -> [StyledCell style cc] -- ^ The cells of the row.
       -> StyledRow style cc
conRow style cells = Styling style (Row cells)

type RowMapFun s cell out
    =  RowGroupType
    -> s
    -> [cell]
    -> out

rowMap :: RowGroupType
       -> RowMapFun    s    cell row
       -> CellMapFun   s cc cell
       -> StyledRow s cc
       -> row
rowMap rgt rowMap' cellMap' (Styling style (Row cs)) =
    let cs' = map (cellMap rgt cellMap') cs
    in  rowMap' rgt style cs'

data HeaderDirection = RowDirection | ColumnDirection

data CellType
  = DataCell
  | HeaderCell HeaderDirection

dataCellType  :: CellType
rowHeaderType :: CellType -- ^ Header for a row
colHeaderType :: CellType -- ^ Header for a column
dataCellType  = DataCell
rowHeaderType = HeaderCell RowDirection
colHeaderType = HeaderCell ColumnDirection

data Cell contentType = Cell
    {
      cellType    :: CellType
    , cellSpan    :: Span        -- ^ (colspan > 1,rowspan > 1)
    , cellContent :: contentType  -- ^ The content of the cell.
    }

type StyledCell style contentType = Styling style (Cell contentType)

type CellMapFun s cc out = RowGroupType -> s -> Cell cc -> out

cellMap :: RowGroupType
        -> CellMapFun s cc cell
        -> StyledCell s cc
        -> cell
cellMap rgt cellMapF (Styling s cell) = cellMapF rgt s cell

-- | Conucts a table cell.
conCell :: style -- ^ The style of the cell.
        -> CellType
        -> Span  -- ^ Column- and rowspan of the cell.
        -> cc    -- ^ The contents of the cell.
        -> StyledCell style cc
conCell style type_ span cc =
  Styling style $
  Cell
    {
      cellType    = type_
    , cellSpan    = span
    , cellContent = cc
    }

-- | Specification for a group of columns of a table.
-- A table does not know about this kind of information, so these are just for
-- mimicing column groups together with the function 'applyColGroups'.
data ColGroup style = ColGroup
    {
      cgSpan  :: Int,
      cgStyle :: style
    }

-- | Mimics column groups (together with the type 'ColGroup').
--
-- ColGroup and Cell spans can make the borders of groups and cells
-- mismatch.  This means that some decision has to be made about how to treat
-- these cases.  The decision is that a cell gets settings from the ColGroup
-- that applies to the first column of the cell.
-- This means that, for cells that spans more than one column, it is possible
-- that the leftmost part of the cell may get settings that "should" not apply
-- to it.
--
-- The list of 'ColGroup's is allowd to span more columns that the list of
-- Cell specfies.
applyColGroupsToRowCells :: (STYLE s,STYLING c) =>
                            [ColGroup s]
                         -> [c s (Cell a)]
                         -> [c s (Cell a)]
applyColGroupsToRowCells []                    []       = []
applyColGroupsToRowCells []                    ccs      = ccs
applyColGroupsToRowCells (cg : cgs)            []       = [] -- error "List of ColGroup defines more columns than there are."
applyColGroupsToRowCells (ColGroup 0 _  : cgs) ccs      = applyColGroupsToRowCells cgs ccs
applyColGroupsToRowCells (ColGroup n s : cgs) (c : cs) =
    let (m,_) = cellSpan $ getStyled c
        styledCell = addStyleToSTYLING s c
    in  if n >= m
        then styledCell : applyColGroupsToRowCells (ColGroup (n-m) s : cgs) cs
        else styledCell : skipSomeCols (m-n) cgs cs
    where
      -- | Handles the case where the borders of a ColGroup and Cell mismatch.
      -- Skips groups until the one applying to the next cell appears.  Then
      -- also adjusts the length of the ColGroup to compensate for eventual overlapping.
      skipSomeCols :: (STYLE s,STYLING c) =>
                      Int
                   -> [ColGroup s]
                   -> [c s (Cell a)]
                   -> [c s (Cell a)]
      skipSomeCols 0 cgs ccs = applyColGroupsToRowCells cgs ccs
      skipSomeCols _ []  ccs = ccs
      skipSomeCols _ _   []  = []
      skipSomeCols k (ColGroup n cs : cgs) ccs
          | n == k    = applyColGroupsToRowCells cgs ccs
          | n >  k    = applyColGroupsToRowCells (ColGroup (n-k) cs : cgs) ccs
          | otherwise = skipSomeCols (k-n) cgs ccs


-- | Applies the style of the 'ColGroup's to the cells, and removes them from
-- the RowGroup.
applyColGroupsToRowGroupCells
  :: STYLE s
  => RowGroup s a
  -> RowGroup s a
applyColGroupsToRowGroupCells (RowGroup cgs rows) = RowGroup [] $ map (applyCg cgs) rows
    where
      applyCg :: STYLE s =>
                 [ColGroup s]
              -> Styling s (Row s a)
              -> Styling s (Row s a)
      applyCg cgs (Styling s (Row cells)) = Styling s $ Row $ applyColGroupsToRowCells cgs cells

applyColGroupsToCells :: STYLE s
                      => StyledTable s a
                      -> StyledTable s a
applyColGroupsToCells (Styling style (Table
                                      {
                                        tblHead = tblHead,
                                        tblFoot = tblFoot,
                                        tblBody = tblBody
                                      }
                                     )) =
    let head            = applyToMb tblHead
        foot            = applyToMb tblFoot
        Styling sb body' = tblBody
        body            = Styling sb $ applyColGroupsToRowGroupCells body'
    in  Styling style (Table
                      {
                        tblHead = head,
                        tblFoot = foot,
                        tblBody = body
                      }
                     )
    where
      applyToMb Nothing = Nothing
      applyToMb (Just (Styling s row)) = Just $ Styling s $ applyColGroupsToRowGroupCells row
