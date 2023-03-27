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


import           Wilde.WildeUi.WildeComponent
import           Wilde.WildeUi.StdValueTypes as SVT
import           Wilde.WildeUi.WildeTables

import           Wilde.WildeUi.UiPrimitives
import qualified Wilde.WildeUi.WildeStyle as WS
import           Wilde.WildeUi.WildeTable

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
    getHeaderValueRowList :: Presentation.Monad [(Title,AnySVALUE)]
    getHeaderValueRowList = sequence $ mapAttributeAnyValue headerValueRow attrs

    headerValueRow :: ATTRIBUTE_PRESENTATION atConf
                   => Attribute atConf dbTable typeForExisting typeForCreate
                   -> Presentation.Monad (Title,AnySVALUE)
    headerValueRow (Attribute at val getPresVal) =
     do
       presVal <- getPresVal
       pure (theTitle,presVal)
      where
        theTitle = wildeStyled . atTitle $ at
        presO = atPresentationO at

    renderTitle :: (Title,AnySVALUE) -> AnySVALUE
    renderTitle  = AnySVALUE . SVT.UnquotedStringValue . fst

    renderValue :: (Title,AnySVALUE) -> AnySVALUE
    renderValue  = snd


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
