-- | Functionality for presenting a list of Wilde objects

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Wilde.ApplicationConstruction.Presentation.ObjectList
(
  ObjectTypeSetup(..),

  FooterRows,
  FooterRowsConstructor,

  Config(..),
  ObjectList(..),
  DataRows(..),
  ObjectRow(..),

  bodyIsEmpty,

  objectList,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.Accumulator as Acc

import           Wilde.GenericUi.AbstractTable (ColGroup)

import           Wilde.WildeUi.StdValueTypes as SVT

import qualified Wilde.Media.Presentation as Presentation

import           Wilde.ObjectModel.ObjectModelUtils as OmUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


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

data DataRows = DataRows
  {
    listObjects    :: [ObjectRow]
  , listFooterRows :: FooterRows
  }

data ObjectRow = ObjectRow
  {
    rowAttributes :: [AnySVALUE]
  , rowActions    :: [AnySVALUE]
  -- ^ Length of this list equals the length of the list of
  -- row action constructors given.
  -- Also, the order of the elements corresponds to the order of that list.
  }

type FooterRows = ([ColGroup WildeStyle],[[WildeCell]])

type FooterRowsConstructor object = Acc.Accumulator object FooterRows

data ObjectTypeSetup object idAt = ObjectTypeSetup
  {
    otsGetId      :: object -> idAt
  , otsAttrTitles :: [StyledTitle]
  , otsGettAttrs  :: object -> Presentation.Monad [AnySVALUE]
  }

-- | Produces a table that shows a list of 'Object's.
objectList
  :: forall object idAt.
     Maybe StyledTitle
  -> ObjectTypeSetup object idAt
  -> FooterRowsConstructor object
  -> [idAt -> AnySVALUE]
  -- ^ List of "action columns" corresponding to a given object.
  -- These are columns that can contain buttons e.g. (but really anything,
  -- of course).
  -- The data is delivered in the `ObjectRow`.
  -> Presentation.Monad [object]
  -> Presentation.Monad ObjectList
objectList mbTitle  otSetup
  footerConstructor listOfMkObjectAction getObjects = ObjectList config <$> getDataRows
  where
    config              :: Config
    config               = Config mbTitle attrTitles

    attrTitles           = otsAttrTitles otSetup

    getDataRows         :: Presentation.Monad DataRows
    getDataRows = do
      os <- getObjects
      objectRows <- mapM newObjectRow os
      let footerRows = Acc.resultOfSum footerConstructor os
      pure $ DataRows objectRows footerRows

    mkObjectActions    :: idAt -> [AnySVALUE]
    mkObjectActions id  = map (\f -> f id) listOfMkObjectAction

    newObjectRow         = objectRow (otsGettAttrs otSetup)

    objectRow :: (object -> Presentation.Monad [AnySVALUE])
              -> object
              -> Presentation.Monad ObjectRow
    objectRow getDisplayAttrs o =
      do
        attributeValues <- getDisplayAttrs o
        pure $ ObjectRow attributeValues rowActions
      where
        idAttrValue       :: idAt
        idAttrValue        = otsGetId otSetup o

        rowActions        :: [AnySVALUE]
        rowActions         = mkObjectActions idAttrValue
