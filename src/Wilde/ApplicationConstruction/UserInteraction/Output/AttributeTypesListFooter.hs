{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------
-- | Tools for constructing the footer rows of an 'Object' list, that can be used
-- by 'ObjectListSetup'.
--
-- \"Footer cell constructors\" are specified for individual 'AttributeType's.
-- These \"constructors\" construct cells that are displayed in the table column
-- of the 'AttributeType'.
-- If the 'AttributeType' is not displayed in the list, then the footer cells are not
-- displayed either.
--
-- /USAGE/
--
-- In a \"top-down\" approach:
--
-- 1. Construct a 'ObjListSetup.GetMkFooterRowsConstructor' using
--    'attributeTypesFooterCells'.
--   ('ObjListSetup.GetMkFooterRowsConstructor' is what is used by
--   'ObjListSetup.ObjectListDisplaySetup'.)
--
-- 1. Construct the 'AttributeTypesFooterSpecification' to give to
--    'attributeTypesFooterCells'.
--
-- 1. An 'AttributeTypesFooterSpecification' is just a list of associations
--    between an 'AttributeType' and \"cells constructors\".
--
-- 1. As \"cells constructs\", either use the ones predefined in this module
--    (e.g. 'sumCell', 'numObjectsCell'), or implement your own.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.Output.AttributeTypesListFooter
       (
         -- * Basic types and functions

         attributeTypesFooterCellsGetter,
         AttributeTypesFooterSpecification(..),

         AttributeTypeFooterSpecification(..),

         AnyCellConstructor(..),
         CellConstructor(..),

         -- * Utilities

         mkAtFooterSpec,

         toOptional,
         toOptionalRaw,

           -- * Some predefined \"cells constructors\"

         numObjectsCell,

         sumCell,
         sumCell_format,
         sumCell_show,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.List (transpose)
import qualified Data.Map as Map

import qualified Wilde.Utils.Accumulator as Acc

import           Wilde.WildeUi.StdValueTypes
import           Wilde.WildeUi.WildeTables (dataCellStd, dataCellStdEmpty)

import           Wilde.ObjectModel.ObjectModel
import           Wilde.ObjectModel.ObjectModelUtils (castToAttributeType,anyValueApply,anyValueApply2)
import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import qualified Wilde.ObjectModel.Presentation.FooterRowsConstructor as F
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as ObjListSetup (GetMkFooterRowsConstructor)
import           Wilde.WildeUi.WildeTable


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Footers for individual AttributeType:s -
-------------------------------------------------------------------------------


-- | Constructs a 'ObjListSetup.GetMkFooterRowsConstructor' that can be used
-- by 'ObjListSetup.ObjectListDisplaySetup'.
--
-- Rows are constructed from specifications of individual 'AttributeType's.
-- For each 'AttributeType' that is included in the specification, a list of
-- cells is constructed, that is displayed in the same column as the
-- 'AttributeType'.
attributeTypesFooterCellsGetter
  :: forall otConf atConf dbTable otNative idAtE idAtC.
     ObjectType otConf atConf dbTable otNative idAtE idAtC
  -> AttributeTypesFooterSpecification atConf dbTable
  -> ObjListSetup.GetMkFooterRowsConstructor otConf atConf dbTable otNative idAtE idAtC
attributeTypesFooterCellsGetter _ [] = pure F.mkNoFooterRows
attributeTypesFooterCellsGetter ot atCellConstructors = pure retVal
  where
    retVal :: F.MkFooterConstructor otConf atConf dbTable otNative idAtE idAtC
    retVal atsInList =
      Acc.accumulatorWithCount
        atCellConstructors
        (accumulateAllAts (getAttrsForConstructors ot atCellConstructors))
        (makeFooter atsInList)

    getAttrsForConstructors :: ObjectType otConf atConf dbTable otNative idAtE idAtC
                            -> AttributeTypesFooterSpecification atConf dbTable
                            -> Object otConf atConf dbTable otNative idAtE idAtC
                            -> [Any (Attribute atConf dbTable)]
    getAttrsForConstructors ot atsfs =
      AttributeTypeListSetup.apply $
      either
      (\(Any at) -> errorNoSuchAtInOt at)
      id
      (AttributeTypeListSetup.mk ot $ footerAttributeTypes atsfs)
      where
        errorNoSuchAtInOt at = error $ "ObjectType " ++
                               otCrossRefKey ot ++
                               " does not contain AttributeType " ++
                               atCrossRefKey at

    accumulateAllAts :: (Object otConf atConf dbTable otNative idAtE idAtC
                         -> [Any (Attribute atConf dbTable)])
                     -> AttributeTypesFooterSpecification atConf dbTable
                     -> Object otConf atConf dbTable otNative idAtE idAtC
                     -> AttributeTypesFooterSpecification atConf dbTable
    accumulateAllAts getAttrsForAtsFooterSpec atSpecs o =
      map accumulateAt $ zip atSpecs (getAttrsForAtsFooterSpec o)

    accumulateAt :: (Any (AttributeTypeFooterSpecification atConf dbTable),
                     Any (Attribute                        atConf dbTable))
                 -> Any (AttributeTypeFooterSpecification  atConf dbTable)
    accumulateAt (Any (AttributeTypeFooterSpecification
                                {
                                  atfcAttributeType    = at
                                , atfcCellConstructors = cellConstructors
                                }),
                  attr) =
      let
        value = either
                -- TODO Throw errors as "checked" errors via the monad??
                (\convertErr -> error $ "attributeTypesFooterCells: Error casting Attribute: " ++ (show convertErr))
                id
                (castToAttributeType at attr)
      in
       Any $
       AttributeTypeFooterSpecification
       {
         atfcAttributeType    = at
       , atfcCellConstructors = map (accumulateCellsConstructor value) cellConstructors
       }

    accumulateCellsConstructor :: a
                                  -> AnyCellConstructor a
                                  -> AnyCellConstructor a
    accumulateCellsConstructor value (AnyCellConstructor fcc) =
      AnyCellConstructor $ accumulateCellsConstructor' value fcc

    accumulateCellsConstructor' :: a
                                   -> CellConstructor acc a
                                   -> CellConstructor acc a
    accumulateCellsConstructor' value x@(CellConstructor
                                         {
                                           fccInitial     = current
                                         , fccAccumulator = accumulator
                                         }) =
      x { fccInitial = accumulator (value,current) }

    makeFooter atsInList (numObjects, acc) = ([],rows)
      where
        rows           = makeRows atsInList cellsForAtsMap
        cellsForAtsMap = makeCells numObjects acc

    makeCells :: Int
              -> AttributeTypesFooterSpecification atConf dbTable
              -> Map.Map CrossRefIdentifier [WildeCell]
    makeCells numObjects acc = Map.fromListWith appendCellsOfSameAt crossRefKeysAndCells
      where
        appendCellsOfSameAt  = (++)
        crossRefKeysAndCells = map (anyValueApply crossRefKeyAndCells) acc

        crossRefKeyAndCells :: AttributeTypeFooterSpecification atConf t e c
                               -> (CrossRefIdentifier,[WildeCell])
        crossRefKeyAndCells (AttributeTypeFooterSpecification at cellConstructors) =
          (atCrossRefKey at,concatMap getCells cellConstructors)

        getCells :: AnyCellConstructor e -> [WildeCell]
        getCells (AnyCellConstructor cc) =
          (fccMkCell cc) numObjects (fccInitial cc)

    makeRows :: [Maybe (Any (AttributeType atConf dbTable))]
             -> Map.Map CrossRefIdentifier [WildeCell]
             -> [[WildeCell]]
    makeRows []      _           = []
    makeRows columns cellsForAts =
      columnsToRows $ appendToSameLength dataCellStdEmpty columnsOfArbitraryLength
      where
        columnsOfArbitraryLength :: [[WildeCell]]
        columnsOfArbitraryLength = map cellsForColumn columns
        cellsForColumn :: Maybe (Any (AttributeType atConf dbTable)) -> [WildeCell]
        cellsForColumn Nothing                = []
        cellsForColumn (Just (Any at)) = maybe [] id $
                                                Map.lookup (atCrossRefKey at) cellsForAts
        appendToSameLength :: a -> [[a]] -> [[a]]
        appendToSameLength e xss = map (appendToLength e maxLength) xss
          where
            maxLength = maximum $ map length xss
            appendToLength :: a -> Int -> [a] -> [a]
            appendToLength _ 0 xs     = xs
            appendToLength e n (x:xs) = x : appendToLength e (n-1) xs
            appendToLength e n []     = replicate n e

    columnsToRows :: [[WildeCell]]
                  -> [[WildeCell]]
    columnsToRows columnsOfEqualLength = transpose columnsOfEqualLength


footerAttributeTypes :: AttributeTypesFooterSpecification atConf dbTable
                        -> [Any (AttributeType atConf dbTable)]
footerAttributeTypes = map (anyValueApply2 atfcAttributeType)

-- | Lists the 'AttributeType's that should have footer cells.
--
-- (The @dbTable@ type parameter forces the 'AttributeType's to belong to
-- the same 'ObjectType'.)
type AttributeTypesFooterSpecification atConf dbTable =
  [AnyAttributeTypeFooterSpecification atConf dbTable]

type AnyAttributeTypeFooterSpecification atConf dbTable =
  Any (AttributeTypeFooterSpecification atConf dbTable)

-- | Utility for constructing an 'AnyAttributeTypeFooterSpecification'
mkAtFooterSpec :: AttributeType atConf dbTable typeForExisting typeForCreate
               -> [AnyCellConstructor typeForExisting]
               -> AnyAttributeTypeFooterSpecification atConf dbTable
mkAtFooterSpec at cellsConstructors =
  Any $
  AttributeTypeFooterSpecification at cellsConstructors

-- | Specifies the footer cells for a single 'AttributeType'
data AttributeTypeFooterSpecification atConf dbTable typeForExisting typeForCreate =
  AttributeTypeFooterSpecification
  {
    atfcAttributeType    :: AttributeType atConf dbTable typeForExisting typeForCreate
  , atfcCellConstructors :: [AnyCellConstructor typeForExisting]
  }

-------------------------------------------------------------------------------
-- | Hides the accumulator type of a 'CellConstructor', so that
-- 'CellConstructor's can be put in the same list.
--
-- The parameter @typeForExisting@ is the type of value of the 'AttributeType'.
-- It is exposed to force lists to contain only 'CellConstructor's
-- for a single 'AttributeType'.
-------------------------------------------------------------------------------
data AnyCellConstructor typeForExisting =
  forall acc . AnyCellConstructor (CellConstructor acc typeForExisting)

-------------------------------------------------------------------------------
-- | The type of a \"concrete\" \"cells constructor\" for a 'AttributeType'.
--
-- @typeForExisting@ is the type of value of the 'AttributeType'.
--
-- Defines how to construct cells by first \"accumulating\" the values of all
-- 'Attribute's in the list. The values are accumulated into a value of a type
-- that suits the purpose of constructing the cells.
--
-- Accumulation is done in the manner of the \"fold\" functions.
-------------------------------------------------------------------------------
data CellConstructor acc typeForExisting =
  CellConstructor
  {
    -- | Initial accumulator value.
    fccInitial     :: acc
    -- | Accumulates an 'Attribute'.
  , fccAccumulator :: (typeForExisting,acc) -> acc
    -- | Constructs the cells, given the number of 'Object's in the list
    -- and the accumulation of all 'Attribute's in the list.
  , fccMkCell      :: Int -> acc -> [WildeCell]
  }


-------------------------------------------------------------------------------
-- - Utilities -
-------------------------------------------------------------------------------


-- | Transforms a 'CellConstructor' for a mandatory value to one for an
-- optional value.
--
-- Maybe:s are silently ignored.
--
-- The number of objects passed to the rows constructor is not changed.
--
toOptionalRaw :: CellConstructor acc typeForExisting
              -> CellConstructor acc (Maybe typeForExisting)
toOptionalRaw cc = cc { fccAccumulator = newAcc }
  where
    newAcc = \(mbX,acc) -> maybe acc (\x -> oldAcc (x,acc)) mbX
    oldAcc = fccAccumulator cc

-- | Transforms a 'CellConstructor' for a mandatory value to one for an
-- optional value.
--
-- Maybe:s are silently ignored.
--
-- The number of objects passed to the rows constructor is not changed.
--
toOptional :: AnyCellConstructor typeForExisting
           -> AnyCellConstructor (Maybe typeForExisting)
toOptional (AnyCellConstructor cc) = AnyCellConstructor $ toOptionalRaw cc


-------------------------------------------------------------------------------
-- - Predefined cells constructors -
-------------------------------------------------------------------------------


-- | Displays the sum of all 'Attribute's.
sumCell_show :: (Num a, Show a) => AnyCellConstructor a
sumCell_show = sumCell_format show

-- | Displays the sum of all 'Attribute's.
sumCell_format :: Num a => (a -> String) -> AnyCellConstructor a
sumCell_format formatter = sumCell (dataCellStd . UnquotedStringValue . formatter)

-- | Displays the sum of all 'Attribute's.
sumCell :: Num a => (a -> WildeCell) -> AnyCellConstructor a
sumCell mkCell = AnyCellConstructor $ sumCell' mkCell

sumCell' :: Num a => (a -> WildeCell) -> CellConstructor a a
sumCell' mkCell =
  CellConstructor
  {
    fccInitial     = fromInteger 0
  , fccAccumulator = uncurry (+)
  , fccMkCell      = \numObjects sum -> if numObjects > 1
                                        then [mkCell sum]
                                        else []
  }

-- | Displays the number of 'Object's in the list.
--
-- (Can be used for any 'AttributeType', since it is totally independent of
-- the type of value that the 'AttributeType' represents.)
numObjectsCell :: AnyCellConstructor a
numObjectsCell = AnyCellConstructor numObjectsCell'

numObjectsCell' :: CellConstructor () a
numObjectsCell' =
  CellConstructor
  {
    fccInitial     = ()
  , fccAccumulator = const ()
  , fccMkCell      = \numObjects _ -> [dataCellStd $ IntValue numObjects]
  }
