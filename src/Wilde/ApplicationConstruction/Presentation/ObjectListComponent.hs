{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ApplicationConstruction.Presentation.ObjectListComponent
(
  objectList,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Maybe (isJust, catMaybes)

import qualified Wilde.GenericUi.AbstractTable as AbstrTbl

import           Wilde.WildeUi.StdValueTypes as SVT
import qualified Wilde.WildeUi.TableUtils as TU

import qualified Wilde.Media.WildeStyle as WS
import qualified Wilde.Media.Presentation as Presentation

import           Wilde.ObjectModel.ObjectModelUtils as OmUtils

import qualified Wilde.Render.AbstractTableToHtml as AbstractTableToHtml

import qualified Wilde.ApplicationConstruction.Presentation.ObjectList as OL
import qualified Wilde.ApplicationConstruction.Presentation.ButtonSequenceValue as BtnSeq


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


objectList
  :: forall object.
     WildeStyle
  -> Maybe StyledTitle
  -> OL.ObjectTypeSetup object
  -> OL.FooterRowsConstructor object
  -> [Presentation.Monad (object -> AnySVALUE)]
  -- ^ Elements shown in the  left-most column, if non-empty
  -> [Presentation.Monad (object -> AnySVALUE)]
  -- ^ Elements shown in the right-most column, if non-empty
  -> Presentation.Monad [object]
  -> Presentation.Monad AnyCOMPONENT
objectList tableStyle mbTitle
  otSetup footerConstructor
  getMkActionsLeft getMkActionsRight
  getObjects =
  do
    (hasLRSideActionColumn, listOfMkObjectAction) <- getSideActionColsSetup
    theObjectList <- OL.objectList mbTitle otSetup footerConstructor listOfMkObjectAction getObjects
    pure $ AnyCOMPONENT $ ObjectListComponent tableStyle hasLRSideActionColumn theObjectList
  where

    getSideActionColsSetup :: Presentation.Monad (HasLRSideActionColumn, [object -> AnySVALUE])
    getSideActionColsSetup = do
      mbMkActionsLeft <- getMkSideContent getMkActionsLeft
      mbMkActionRight <- getMkSideContent getMkActionsRight
      let listOfMkObjectAction = catMaybes [mbMkActionsLeft, mbMkActionRight] :: [object -> AnySVALUE]
      let hasLRSideActionColumn = (isJust mbMkActionsLeft, isJust mbMkActionRight)
      pure $ (hasLRSideActionColumn, listOfMkObjectAction)

    getMkSideContent :: [Presentation.Monad (object -> AnySVALUE)]
                     -> Presentation.Monad (Maybe (object -> AnySVALUE))
    getMkSideContent [] = pure Nothing
    getMkSideContent getMkButtons = do
      mkButtons <- sequence getMkButtons
      pure $ Just $ mkSideContent mkButtons

    mkSideContent :: [object -> AnySVALUE] -> object -> AnySVALUE
    mkSideContent mkButtons o = BtnSeq.new $ map (\mkButton -> mkButton o) mkButtons


type HasLRSideActionColumn = (Bool, Bool)

data ObjectListComponent = ObjectListComponent WildeStyle HasLRSideActionColumn OL.ObjectList

instance COMPONENT ObjectListComponent where
  componentHtml (ObjectListComponent tableStyle hasLRSideRow ol@(OL.ObjectList config dataRows)) =
    AbstractTableToHtml.renderTable styledTable
    where
      styledTable :: WildeTable
      styledTable  = addStyleToSTYLING tableStyle unstyledTable

      unstyledTable :: WildeTable
      unstyledTable = mkTable tableLayouter (OL.attributeTitles config) dataRows

      mkTable :: TableLayouter -> [StyledTitle] -> OL.DataRows -> WildeTable
      mkTable = if OL.bodyIsEmpty ol
                then emptyTable
                else nonEmptyTable hasLRSideRow

      tableLayouter :: TableLayouter
      tableLayouter = TU.conWildeHeaderRowTable2 WS.multiRow (OL.listTitle config)

type TableLayouter = [StyledTitle] -> Maybe OL.FooterRows -> [[WildeCell]] -> WildeTable

emptyTable :: TableLayouter -> [StyledTitle] -> OL.DataRows -> WildeTable
emptyTable tableLayouter attrTitles dataRows = tableLayouter attrTitles Nothing []

nonEmptyTable :: HasLRSideActionColumn -> TableLayouter -> [StyledTitle] -> OL.DataRows -> WildeTable
nonEmptyTable hasLRActionsCols@(hasLeftActionsCol, hasRightActionsCol)
              tableLayouter attrTitles dataRows =
  tableLayouter colTitles mbFooter body
  where
    colTitles      :: [StyledTitle]
    colTitles       = mbActionsColTitle hasLeftActionsCol <>
                      attrTitles <>
                      mbActionsColTitle hasRightActionsCol

    mbActionsColTitle :: Bool -> [StyledTitle]
    mbActionsColTitle False = []
    mbActionsColTitle _     = [buttonColTitle]

    buttonColTitle :: StyledTitle
    buttonColTitle  = wildeStyling WS.objectButtonStyle ""

    mbFooter      :: Maybe OL.FooterRows
    mbFooter       = if footerIsEmpty
                     then Nothing
                     else Just footer

    footer        :: OL.FooterRows
    footer         = OL.listFooterRows dataRows

    footerIsEmpty :: Bool
    footerIsEmpty  = null $ snd footer

    body          :: [[WildeCell]]
    body           = map mkBodyRow $ OL.listObjects dataRows

    actionCells   :: [AnySVALUE] -> ([WildeCell], [WildeCell])
    actionCells    = getActionCells hasLRActionsCols

    mkBodyRow     :: OL.ObjectRow -> [WildeCell]
    mkBodyRow (OL.ObjectRow attributes actions) =
      actionsLeft <> attributeCells <> actionsRight

      where
        actionsLeft, actionsRight :: [WildeCell]
        (actionsLeft, actionsRight) = actionCells actions

        attributeCells  :: [WildeCell]
        attributeCells   = map mkAttributeCell attributes

mkAttributeCell :: AnySVALUE -> WildeCell
mkAttributeCell attr = wildeCellFromSVALUE AbstrTbl.dataCellType AbstrTbl.spanSingle attr

getActionCells :: HasLRSideActionColumn -> [AnySVALUE] -> ([WildeCell], [WildeCell])
getActionCells (True, True)  [l,r] = ([mkActionCell l], [mkActionCell r])
getActionCells (True, False) [l]   = ([mkActionCell l], [])
getActionCells (False, True) [r]   = ([], [mkActionCell r])
getActionCells _             []    = ([], [])

mkActionCell :: AnySVALUE -> WildeCell
mkActionCell  = AbstrTbl.conCell WS.objectButtonStyle AbstrTbl.rowHeaderType AbstrTbl.spanSingle . hideStyleAny
