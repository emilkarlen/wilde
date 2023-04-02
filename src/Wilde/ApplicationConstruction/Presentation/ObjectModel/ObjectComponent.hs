{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectComponent
(
  showOneComponent,
  getShowOneComponent,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.Presentation as Presentation

import           Wilde.WildeUi.StdValueTypes as SVT
import           Wilde.WildeUi.WildeTables
import           Wilde.WildeUi.UiPrimitives
import qualified Wilde.WildeUi.WildeStyles as WS
import           Wilde.WildeUi.WildeTable

import           Wilde.ObjectModel.ObjectModelUtils as OmUtils
import           Wilde.ObjectModel.Presentation (ATTRIBUTE_PRESENTATION(..))
import qualified Wilde.ObjectModel.Presentation as OmPres

import qualified Wilde.Render.AbstractTableToHtml


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


getShowOneComponent :: ATTRIBUTE_PRESENTATION atConf
                    => (Object otConf atConf dbTable otNative idAtExisting idAtCreate
                      -> [Any (Attribute atConf dbTable)])
                    -> Object  otConf atConf dbTable otNative idAtExisting idAtCreate
                    -> Presentation.Monad AnyCOMPONENT
getShowOneComponent getAttrs o =
  do
    let attrs = getAttrs o
    headerAndValueList <- sequence $ mapAttributeAnyValue headerAndValueOf attrs
    pure $ showOneComponent style headerAndValueList
  where
    style            :: WildeStyle
    style             = OmPres.objectTypeStyle_o o
    headerAndValueOf :: ATTRIBUTE_PRESENTATION atConf
                     => Attribute atConf dbTable typeForExisting typeForCreate
                     -> Presentation.Monad (Title,AnySVALUE)
    headerAndValueOf (Attribute at val getPresVal) =
     do
       presVal <- getPresVal
       pure (theTitle,presVal)
      where
        theTitle = wildeStyled . atTitle $ at
        presO = atPresentationO at

showOneComponent :: WildeStyle -> [(Title,AnySVALUE)] -> AnyCOMPONENT
showOneComponent componentStyle headerAndValueList =
  AnyCOMPONENT $ ShowOneComponent componentStyle headerAndValueList

data ShowOneComponent = ShowOneComponent WildeStyle [(Title,AnySVALUE)]

instance COMPONENT ShowOneComponent where
  componentHtml (ShowOneComponent componentStyle headerAndValueList) =
    Wilde.Render.AbstractTableToHtml.renderTable styledTable
    where
      styledTable   :: WildeTable
      styledTable   = addStyleToSTYLING (WS.presentationTableSingle <> componentStyle) unstyledTable
      unstyledTable :: WildeTable
      unstyledTable  = headerValueTable
        renderTitle
        renderValue
        WS.weAttribute
        WS.tableColumnStylesShowOne
        headerAndValueList

      renderTitle :: (Title,AnySVALUE) -> AnySVALUE
      renderTitle  = AnySVALUE . SVT.UnquotedStringValue . fst

      renderValue :: (Title,AnySVALUE) -> AnySVALUE
      renderValue  = snd
