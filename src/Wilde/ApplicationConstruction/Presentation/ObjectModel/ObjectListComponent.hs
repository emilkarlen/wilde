{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectListComponent
(
  objectList,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Render.Html.Types


import qualified Wilde.GenericUi.AbstractTable as AbstrTbl

import           Wilde.WildeUi.StdValueTypes as SVT
import qualified Wilde.WildeUi.TableUtils as TU

import qualified Wilde.Media.WildeStyle as WS
import qualified Wilde.Media.Presentation as Presentation

import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.ObjectModelUtils as OmUtils
import           Wilde.ObjectModel.Presentation (ATTRIBUTE_PRESENTATION(..))
import           Wilde.ObjectModel.Presentation.FooterRowsConstructor2
                   ( FooterConstructor, MkFooterConstructor )

import qualified Wilde.Render.AbstractTableToHtml as AbstractTableToHtml

import qualified Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectList as OL
import qualified Wilde.ApplicationConstruction.Presentation.ButtonSequenceValue as BtnSeq


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


objectList
  :: forall otConf atConf dbTable otNative idAtExisting idAtCreate.
     ATTRIBUTE_PRESENTATION atConf
  => WildeStyle
  -> AttributeTypeListSetup.Setup         otConf atConf dbTable otNative idAtExisting idAtCreate
  -> MkFooterConstructor                  otConf atConf dbTable otNative idAtExisting idAtCreate
  -> [Presentation.Monad (idAtExisting -> AnySVALUE)]
  -> [Presentation.Monad (idAtExisting -> AnySVALUE)]
  -> Maybe StyledTitle
  -> [Object                              otConf atConf dbTable otNative idAtExisting idAtCreate]
  -> Presentation.Monad AnyCOMPONENT
objectList tableStyle atListSetup mkFooterConstructor getMkActionsLeft getMkActionsRight mbTitle os =
  do
    mkActionsLeft <- getMkSideContent getMkActionsLeft
    mkActionRight <- getMkSideContent getMkActionsRight
    theObjectList <- OL.objectList atListSetup footerConstructor mkActionsLeft mkActionRight mbTitle os
    pure $ AnyCOMPONENT $ ObjectListComponent tableStyle theObjectList
  where
    getMkSideContent :: [Presentation.Monad (idAtExisting -> AnySVALUE)]
                     -> Presentation.Monad (idAtExisting -> AnySVALUE)
    getMkSideContent getMkButtons = do
      mkButtons <- sequence getMkButtons
      pure $ mkSideContent mkButtons

    mkSideContent :: [idAtExisting -> AnySVALUE] -> idAtExisting -> AnySVALUE
    mkSideContent mkButtons pk = BtnSeq.new $ map (\mkButton -> mkButton pk) mkButtons

    footerConstructor :: FooterConstructor otConf atConf dbTable otNative idAtExisting idAtCreate
    footerConstructor = mkFooterConstructor colInfos
      where
        colInfos       = Nothing : atColInfos ++ [Nothing]
        atColInfos     = map Just attributeTypes
        attributeTypes = AttributeTypeListSetup.getAts atListSetup


data ObjectListComponent = ObjectListComponent WildeStyle OL.ObjectList

instance COMPONENT ObjectListComponent where
  componentHtml :: ObjectListComponent -> Html
  componentHtml (ObjectListComponent tableStyle ol@(OL.ObjectList config dataRows)) =
    AbstractTableToHtml.renderTable styledTable
    where
      styledTable :: WildeTable
      styledTable  = addStyleToSTYLING tableStyle unstyledTable

      unstyledTable :: WildeTable
      unstyledTable = mkTable tableLayouter (OL.attributeTitles config) dataRows

      mkTable :: TableLayouter -> [StyledTitle] -> OL.DataRows -> WildeTable
      mkTable = if OL.bodyIsEmpty ol
                then emptyTable
                else nonEmptyTable

      tableLayouter :: TableLayouter
      tableLayouter = TU.conWildeHeaderRowTable2 WS.multiRow (OL.listTitle config)

type TableLayouter = [StyledTitle] -> Maybe OL.FooterRows -> [[WildeCell]] -> WildeTable

emptyTable :: TableLayouter -> [StyledTitle] -> OL.DataRows -> WildeTable
emptyTable tableLayouter attrTitles dataRows = tableLayouter attrTitles Nothing []

nonEmptyTable :: TableLayouter -> [StyledTitle] -> OL.DataRows -> WildeTable
nonEmptyTable tableLayouter attrTitles dataRows = tableLayouter colTitles mbFooter body
  where
    colTitles      :: [StyledTitle]
    colTitles       = buttonColTitle : attrTitles ++ [buttonColTitle]

    buttonColTitle :: StyledTitle
    buttonColTitle  = wildeStyling WS.objectButtonStyle ""
    -- buttonColTitle  = neutralTitle ""

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

mkBodyRow :: OL.ObjectRow -> [WildeCell]
mkBodyRow (OL.ObjectRow actionsLeft actionsRight attributes) =
  mkActionCell actionsLeft : attributeCells <> [mkActionCell actionsRight]

  where
    cellActionsLeft, cellActionsRight :: AnyVALUE
    cellActionsLeft  = hideStyleAny actionsLeft
    cellActionsRight = hideStyleAny actionsRight

    mkActionCell    :: AnySVALUE -> WildeCell
    mkActionCell     = AbstrTbl.conCell WS.objectButtonStyle AbstrTbl.rowHeaderType AbstrTbl.spanSingle . hideStyleAny

    attributeCells  :: [WildeCell]
    attributeCells   = map mkAttributeCell attributes

    mkAttributeCell :: AnySVALUE -> WildeCell
    mkAttributeCell attr = wildeCellFromSVALUE AbstrTbl.dataCellType AbstrTbl.spanSingle attr
