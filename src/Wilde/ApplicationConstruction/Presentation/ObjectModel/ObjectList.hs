-- | Functionality for presenting a list of Wilde objects

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectList
(
  Config(..),
  ObjectList(..),
  DataRows(..),
  ObjectRow(..),
  FooterRows,

  bodyIsEmpty,

  FooterConstructor,

  objectList,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import qualified Wilde.Utils.Accumulator as Acc

import           Wilde.GenericUi.AbstractTable (CellType(..), ColGroup)

import           Wilde.WildeUi.StdValueTypes as SVT
import           Wilde.WildeUi.TableUtils

import qualified Wilde.Media.Presentation as Presentation

import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.ObjectModelUtils as OmUtils
import           Wilde.ObjectModel.Presentation (ATTRIBUTE_PRESENTATION(..), atTitle)
import           Wilde.ObjectModel.Presentation.FooterRowsConstructor2
                   ( FooterConstructor )
import qualified Wilde.Application.ObjectTypeService as Presentation


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Produces a table that shows a list of 'Object's.
objectList
  :: forall otConf atConf dbTable otNative idAtExisting idAtCreate.
     ATTRIBUTE_PRESENTATION atConf
  => AttributeTypeListSetup.Setup     otConf atConf dbTable otNative idAtExisting idAtCreate
  -> FooterConstructor                otConf atConf dbTable otNative idAtExisting idAtCreate
  -> (idAtExisting -> AnySVALUE)
  -> (idAtExisting -> AnySVALUE)
  -> Maybe StyledTitle
  -> [Object                          otConf atConf dbTable otNative idAtExisting idAtCreate]
  -> Presentation.Monad ObjectList
objectList atListSetup
  footerConstructor mkActionsLeft mkActionsRight mbTitle os = do
    dataRows <- getDataRows
    pure $ ObjectList config dataRows
  where
    config              :: Config
    config               = Config mbTitle attrTitles

    newObjectRow         = objectRow (AttributeTypeListSetup.apply atListSetup)
    attrTitles           = map getAtTitle attributeTypes
    attributeTypes       = AttributeTypeListSetup.getAts atListSetup

    getDataRows         :: Presentation.Monad DataRows
    getDataRows = do
      objectRows <- mapM newObjectRow os
      let footerRows = Acc.resultOfSum footerConstructor os
      pure $ DataRows objectRows footerRows

    objectRow :: (Object otConf atConf dbTable otNative idAtExisting idAtCreate
                  -> [Any (Attribute atConf dbTable)])
              -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
              -> Presentation.Monad ObjectRow
    objectRow getDisplayAttrs o =
      do
        attributeValues <- sequence getAttributeValues
        pure $ ObjectRow leftContent rightContent attributeValues
      where
        idAttrValue       :: idAtExisting
        idAttrValue        = attrValue $ oIdAttribute o

        leftContent        = mkActionsLeft idAttrValue
        rightContent       = mkActionsRight idAttrValue

        getAttributeValues :: [Presentation.Monad AnySVALUE]
        getAttributeValues  = mapAttributeAnyValue attrPresentation $ getDisplayAttrs o


data ObjectList = ObjectList
  {
    listConfig   :: Config
  , listDataRows :: DataRows
  }

bodyIsEmpty :: ObjectList -> Bool
bodyIsEmpty = null . listObjects . listDataRows

data Config = Config
  {
    listTitle       :: Maybe StyledTitle
  , attributeTitles :: [StyledTitle]
  }

type FooterRows = ([ColGroup WildeStyle],[[WildeCell]])

data DataRows = DataRows
  {
    listObjects    :: [ObjectRow]
  , listFooterRows :: FooterRows
  }

data ObjectRow = ObjectRow
  {
    rowActionsLeft  :: AnySVALUE
  , rowActionsRight :: AnySVALUE
  , rowAttributes   :: [AnySVALUE]
  }


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


getAtTitle :: ATTRIBUTE_PRESENTATION atConf
           => Any (AttributeType atConf dbTable) -> StyledTitle
getAtTitle (Any at) = atTitle at
