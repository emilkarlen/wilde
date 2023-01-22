-------------------------------------------------------------------------------
-- | Definitions for constructing \"footer rows\" of a table.
--
-- \"Footer rows\" typicaly contain, e.g. sum of a column that
-- contain numbers.
--
-- The definitions here make it possible to construct arbitrary
-- sucn footer rows - both rows with columns that corresponds
-- to the columns of the table body, and also rows with columns
-- that do not correspond to the boody rows.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Presentation.FooterRowsConstructor
       (
         FooterRowsConstructor(..),
         FooterRowAccumulator,
         FooterRows,

         FooterAccumulationState,
         zeroFooterState,
         succFooterState,
         footerRowsForFooterState,
         footerRowsForObjects,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.WildeUi.StdValueTypes as SVT

import Wilde.GenericUi.AbstractTable
import Wilde.ObjectModel.ObjectModelUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Information for constructing footer rows for a table that displays a
-- list of 'Object's.
data FooterRowsConstructor acc otConf atConf dbTable otNative idAtE idAtC =
  FooterRowsConstructor
  {
    -- | Initial accumulation value (computed before any 'Object's have been read).
    frcInitial     :: acc
    -- | Accumulates the currenct accumulation value with the next 'Object'.
  , frcAccumulator :: (Object otConf atConf dbTable otNative idAtE idAtC,acc) -> acc
    -- | Makes the footer rows from the final accumulated value.
    -- If there should be no footer rows, let the list of rows be empty.
    -- The first argument is the "column setup", with a (Just 'AttribuetType')
    -- for each 'AttributeType' that is displayed.
    -- Nothing values represent columns with something else then 'AttributeType's,
    -- e.g. buttons.
  , frcMkRows      :: [Maybe (Any (AttributeType atConf dbTable))]
                   -> Int -- number of Objects
                   -> acc
                   -> FooterRows
  }

-- | Information enough to display the footer rows.
--
-- The first list is styling of columns.  The length of this list may range from
-- 0 to the total number of columns in the table.
--
-- The second list is a list of rows.  The length of each row-list must be equal
-- to the number of columns in the table.
type FooterRows = ([ColGroup WildeStyle]
                  ,[[WildeStyledCell]])

-- | Accumulates an 'Object' into the accumulated value that is finally used
-- for constructing the footer rows.
type FooterRowAccumulator acc otConf atConf dbTable otNative idAtE idAtC =
  (acc,Object otConf atConf dbTable otNative idAtE idAtC) -> acc

newtype FooterAccumulationState acc otConf atConf dbTable otNative idAtExisting idAtCreate =
  FooterAccumulationState (Int
                          ,acc
                          ,FooterRowsConstructor acc otConf atConf dbTable otNative idAtExisting idAtCreate)

-- | The initial state in the computation of footers using a 'FooterRowsConstructor'.
zeroFooterState :: FooterRowsConstructor   acc otConf atConf dbTable otNative idAtExisting idAtCreate
                -> FooterAccumulationState acc otConf atConf dbTable otNative idAtExisting idAtCreate
zeroFooterState frc = FooterAccumulationState (0,frcInitial frc,frc)

-- | Accumulates one 'Object' in the construction of footer rows.
succFooterState :: FooterAccumulationState acc otConf atConf dbTable otNative idAtExisting idAtCreate
                -> Object                      otConf atConf dbTable otNative idAtExisting idAtCreate
                -> FooterAccumulationState acc otConf atConf dbTable otNative idAtExisting idAtCreate
succFooterState (FooterAccumulationState (numObjects,acc,frc)) o =
  FooterAccumulationState (succ numObjects,
                           frcAccumulator frc (o,acc),
                           frc)

-- | Constructs the footer rows.
footerRowsForFooterState :: [Maybe (Any (AttributeType atConf dbTable))]
                         -> FooterAccumulationState acc otConf atConf dbTable otNative idAtExisting idAtCreate
                         -> FooterRows
footerRowsForFooterState columns (FooterAccumulationState (numObjects,acc,frc)) =
  frcMkRows frc columns numObjects acc

-- | Utility that computes footer rows "directly" using the other methods
-- defined here.
footerRowsForObjects :: [Maybe (Any (AttributeType atConf dbTable))]
                     -> FooterRowsConstructor acc otConf atConf dbTable otNative idAtExisting idAtCreate
                     -> [Object otConf atConf dbTable otNative idAtExisting idAtCreate]
                     -> FooterRows
footerRowsForObjects columns frc os = footerRowsForFooterState columns $ foldl succFooterState zero os
  where
    zero = zeroFooterState frc
