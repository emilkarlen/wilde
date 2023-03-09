-------------------------------------------------------------------------------
-- | Presentation of Object's and 'Attribute's.
--
-- TODO: decompose into smaller pieces.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Presentation
       (
         module Wilde.ObjectModel.Presentation.FooterRowsConstructor,

         ATTRIBUTE_PRESENTATION(..),

         showOneComponent,

         objectListTable,

         -- * Get attributes from objects

         getAttributesInGivenOrder,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.WildeUi.StdValueTypes as SVT
import Wilde.WildeUi.TableUtils
import qualified Wilde.Media.WildeStyle as WS

import qualified Wilde.Media.Presentation as Presentation
import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import Wilde.ObjectModel.ObjectModelUtils

import Wilde.WildeUi.WildeComponent

import Wilde.ObjectModel.Presentation.FooterRowsConstructor


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


class ATTRIBUTE_PRESENTATION atConf where
  atTitle :: AttributeType atConf dbTable typeForExisting typeForCreate
          -> StyledTitle


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
    getHeaderValueRowList :: Presentation.Monad [(Wilde.ObjectModel.ObjectModelUtils.Title,AnySVALUE)]
    getHeaderValueRowList = sequence $ mapAttributeAnyValue headerValueRow attrs

    headerValueRow :: ATTRIBUTE_PRESENTATION atConf
                   => Attribute atConf dbTable typeForExisting typeForCreate
                   -> Presentation.Monad (Wilde.ObjectModel.ObjectModelUtils.Title,AnySVALUE)
    headerValueRow (Attribute at val getPresVal) =
     do
       presVal <- getPresVal
       pure (theTitle,presVal)
      where
        theTitle = wildeStyled . atTitle $ at
        presO = atPresentationO at

    renderTitle :: (Wilde.ObjectModel.ObjectModelUtils.Title,AnySVALUE) -> AnySVALUE
    renderTitle  = AnySVALUE . SVT.UnquotedStringValue . fst

    renderValue :: (Wilde.ObjectModel.ObjectModelUtils.Title,AnySVALUE) -> AnySVALUE
    renderValue  = snd


-------------------------------------------------------------------------------
-- - showMany -
-------------------------------------------------------------------------------


-- | Produces a table that shows a list of 'Object's.

showManyTable :: ATTRIBUTE_PRESENTATION atConf
              => ObjectType otConf atConf dbTable otA idAtExisting idAtCreate
              -> (idAtExisting -> AnySVALUE)
              -> (idAtExisting -> AnySVALUE)
              -> Maybe StyledTitle
              -> [Object otConf atConf dbTable otA idAtExisting idAtCreate]
              -> Presentation.Monad WildeTable
showManyTable ot leftSideContentConstructor rightSideContentConstructor mbTitle os =
  let footerDataZero       = 0 :: Int
      footerDataAcc n rec  = 1 + n
      titles               :: [StyledTitle]
      buttonColTitle       = neutralTitle ""
      attrColsTitle        = map getAtTitle $ otAttributeTypes ot
      titles               = buttonColTitle : attrColsTitle ++ [buttonColTitle]
      mkTable              = conStandardTable mbTitle titles
      mkObjectRow          = objectRow leftSideContentConstructor rightSideContentConstructor
  in  tableWithFooterRowsM footerDataAcc footerDataZero footerColFun mkObjectRow mkTable os
    where
      footerColFun :: Int -> [[WildeStyledCell]]
      footerColFun numRows =
        let
          numObjectsCell = cellStd $ IntValue numRows :: WildeStyledCell
          otherCellsInfo = cellSpaned (2 + numNonIdAts,1) valueEmpty :: WildeStyledCell
          numNonIdAts    = length $ otNonIdAttributeTypes ot
        in
         [[numObjectsCell,otherCellsInfo]]

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
atElementWithStyle attr = fmap SeValue $ attrPresentation attr


-------------------------------------------------------------------------------
-- - object list -
-------------------------------------------------------------------------------


-- | Produces a table that shows a list of 'Object's.
objectListTable :: ATTRIBUTE_PRESENTATION atConf
                => AttributeTypeListSetup.Setup     otConf atConf dbTable otNative idAtExisting idAtCreate
                -> Maybe (FooterRowsConstructor acc otConf atConf dbTable otNative idAtExisting idAtCreate)
                -> (idAtExisting -> AnySVALUE)
                -> (idAtExisting -> AnySVALUE)
                -> Maybe StyledTitle
                -> [Object                          otConf atConf dbTable otNative idAtExisting idAtCreate]
                -> Presentation.Monad WildeTable
objectListTable atListSetup Nothing leftSideContentConstructor rightSideContentConstructor mbTitle os =
  objectListTable atListSetup (Just footerRowsConstructor) leftSideContentConstructor rightSideContentConstructor mbTitle os
    where
      footerRowsConstructor =
        FooterRowsConstructor
        {
          frcInitial     = ()
        , frcAccumulator = const ()
        , frcMkRows      = \colInfos numObjs acc -> ([],[])
        }

objectListTable atListSetup
  (Just footerRowsConstructor) leftSideContentConstructor rightSideContentConstructor mbTitle os =
    tableWithFooterRowsM footerDataAcc footerDataZero footerColFun mkObjectRow mkTable os
  where
    footerDataZero       = zeroFooterState footerRowsConstructor
    footerDataAcc        = succFooterState
    footerColFun         = \state -> snd $ footerRowsForFooterState colInfos state
    mkTable              = conStandardTable mbTitle titles
    titles               :: [StyledTitle]
    titles               = if tableIsEmpty
                           then attrColsTitle
                           else buttonColTitle : attrColsTitle ++ [buttonColTitle]
    colInfos             = if tableIsEmpty
                           then atColInfos
                           else Nothing : atColInfos ++ [Nothing]
    mkObjectRow          = objectRow' leftSideContentConstructor rightSideContentConstructor
                           (AttributeTypeListSetup.apply atListSetup)
    atColInfos           = map Just attributeTypes
    attrColsTitle        = map getAtTitle attributeTypes
    attributeTypes       = AttributeTypeListSetup.getAts atListSetup
    buttonColTitle      :: StyledTitle
    buttonColTitle       = wildeStyling WS.objectButtonStyle ""
    tableIsEmpty         = null os

objectRow' :: (idAtExisting -> AnySVALUE)
           -> (idAtExisting -> AnySVALUE)
           -> (Object otConf atConf dbTable otNative idAtExisting idAtCreate
               -> [Any (Attribute atConf dbTable)])
           -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
           -> Presentation.Monad [ElementWithStyle]
objectRow' leftSideContentConstructor rightSideContentConstructor getDisplayAttrs o =
  do
    attributeValues <- sequence getAttributeValues
    pure $ leftContentElem : attributeValues ++ [rightContentElem]
  where
    leftContent        = leftSideContentConstructor $ attrValue $ oIdAttribute o
    leftContentElem    = SeValue leftContent
    rightContent       = rightSideContentConstructor $ attrValue $ oIdAttribute o
    rightContentElem   = SeValue rightContent
    getAttributeValues = mapAttributeAnyValue atElementWithStyle $ getDisplayAttrs o


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


getAtTitle :: ATTRIBUTE_PRESENTATION atConf
           => Any (AttributeType atConf dbTable) -> StyledTitle
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
