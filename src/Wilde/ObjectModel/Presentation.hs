-------------------------------------------------------------------------------
-- | Presentation of Object's and 'Attribute's.
--
-- TODO: decompose into smaller pieces.
-------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ObjectModel.Presentation
       (
         module F,

         ATTRIBUTE_PRESENTATION(..),

         showOneComponent,

         -- * Get attributes from objects

         getAttributesInGivenOrder,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.GenericUi.AbstractTable (CellType(..))

import           Wilde.WildeUi.WildeComponent
import           Wilde.WildeUi.StdValueTypes as SVT
import           Wilde.WildeUi.TableUtils

import qualified Wilde.Media.WildeStyle as WS
import qualified Wilde.Media.Presentation as Presentation
import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.ObjectModelUtils as OmUtils
import           Wilde.ObjectModel.Presentation.FooterRowsConstructor as F


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


class ATTRIBUTE_PRESENTATION atConf where
  atTitle :: AttributeType atConf dbTable typeForExisting typeForCreate
          -> WildeTitle


-------------------------------------------------------------------------------
-- - Show One -
-------------------------------------------------------------------------------


showOneComponent :: ATTRIBUTE_PRESENTATION atConf
                 => (Object otConf atConf dbTable otNative idAtExisting idAtCreate
                 -> [Any (Attribute atConf dbTable)])
                 -> Object  otConf atConf dbTable otNative idAtExisting idAtCreate
                 -> Presentation.Monad AnyCOMPONENT
showOneComponent getAttrs o =
  do
    unstyledTable <- showOneTable (getAttrs o)
    let styledTable = addStyleToSTYLING WS.presentationTableSingle unstyledTable
    pure $ AnyCOMPONENT $ TableListComponent styledTable

showOneTable :: ATTRIBUTE_PRESENTATION atConf
             => [Any (Attribute atConf dbTable)]
             -> Presentation.Monad WildeTable
showOneTable attrs =
  do
    headerValueRowList <- getHeaderValueRowList
    pure $
      wildeHeaderValueTable
      renderTitle
      renderValue
      WS.weAttribute
      WS.tableColumnStylesShowOne
      headerValueRowList
  where
    getHeaderValueRowList :: Presentation.Monad [(OmUtils.Title,AnySVALUE)]
    getHeaderValueRowList = sequence $ mapAttributeAnyValue headerValueRow attrs

    headerValueRow :: ATTRIBUTE_PRESENTATION atConf
                   => Attribute atConf dbTable typeForExisting typeForCreate
                   -> Presentation.Monad (OmUtils.Title,AnySVALUE)
    headerValueRow (Attribute at val getPresVal) =
     do
       presVal <- getPresVal
       pure (theTitle,presVal)
      where
        theTitle = wildeStyled . atTitle $ at
        presO = atPresentationO at

    renderTitle :: (OmUtils.Title,AnySVALUE) -> AnySVALUE
    renderTitle  = AnySVALUE . SVT.UnquotedStringValue . fst

    renderValue :: (OmUtils.Title,AnySVALUE) -> AnySVALUE
    renderValue  = snd


-------------------------------------------------------------------------------
-- - showMany -
-------------------------------------------------------------------------------


-- | Produces a table that shows a list of 'Object's.

showManyTable :: forall otConf atConf dbTable otA idAtExisting idAtCreate. ATTRIBUTE_PRESENTATION atConf
              => ObjectType otConf atConf dbTable otA idAtExisting idAtCreate
              -> (idAtExisting -> AnySVALUE)
              -> (idAtExisting -> AnySVALUE)
              -> Maybe WildeTitle
              -> [Object otConf atConf dbTable otA idAtExisting idAtCreate]
              -> Presentation.Monad WildeTable
showManyTable ot leftSideContentConstructor rightSideContentConstructor mbTitle os =
  tableWithFooterRowsM footerDataSetup mkObjectRow mkTable os
    where
      footerDataSetup :: FooterRowsSetup Int (Object otConf atConf dbTable otA idAtExisting idAtCreate)
      footerDataSetup = FooterRowsSetup
        {
          frsAdd        = \n obj -> n + 1
        , frsZero       = 0
        , frsRenderRows = footerColFun
        }

      mkTable              = conStandardTable mbTitle titles
      mkObjectRow          = objectRow leftSideContentConstructor rightSideContentConstructor

      attrColsTitle        = map getAtTitle $ otAttributeTypes ot

      titles               :: [WildeTitle]
      titles               = buttonColTitle : attrColsTitle ++ [buttonColTitle]

      buttonColTitle       = neutralTitle ""

      footerColFun :: Int -> [[WildeCell]]
      footerColFun numRows = [[numObjectsCell,otherCellsInfo]]
        where
          footerColType  = DataCell :: CellType
          numObjectsCell = cellStd    footerColType $ IntValue numRows :: WildeCell
          otherCellsInfo = cellSpaned footerColType (2 + numNonIdAts,1) valueEmpty :: WildeCell
          numNonIdAts    = length $ otNonIdAttributeTypes ot

objectRow :: (idAtExisting -> AnySVALUE)
          -> (idAtExisting -> AnySVALUE)
          -> Object otConf atConf dbTable otA idAtExisting idAtCreate
          -> Presentation.Monad [ElementWithStyle]
objectRow leftSideContentConstructor rightSideContentConstructor o =
  do
    attributeValues <- sequence getAttributeValues
    pure $ leftContentElem : attributeValues ++ [rightContentElem]
  where
    leftContent        = leftSideContentConstructor $ attrValue $ oIdAttribute o
    leftContentElem    = SeValue leftContent
    rightContent       = rightSideContentConstructor $ attrValue $ oIdAttribute o
    rightContentElem   = SeValue rightContent
    getAttributeValues = mapAttributeAnyValue atElementWithStyle $ oAllAttributesAnyValue o


atElementWithStyle :: Attribute atConf dbTable typeForExisting typeForCreate
                   -> Presentation.Monad ElementWithStyle
atElementWithStyle attr = SeValue <$> attrPresentation attr


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


getAtTitle :: ATTRIBUTE_PRESENTATION atConf
           => Any (AttributeType atConf dbTable) -> WildeTitle
getAtTitle (Any at) = atTitle at


-------------------------------------------------------------------------------
-- Gives the 'Attribute's in the given order.
-------------------------------------------------------------------------------


getAttributesInGivenOrder :: [Any (AttributeType atConf dbTable)]
                          -- ^ Specifies the order in which
                          -- the 'Attribute's inputers are listed.
                          -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
                          -> GeneralResult [Any (Attribute atConf dbTable)]
getAttributesInGivenOrder attributeTypesOrder o =
  do
    atListSetup <- AttributeTypeListSetup.mkGeneral ot attributeTypesOrder
    pure $ AttributeTypeListSetup.apply atListSetup o
  where
    ot = oType o
