{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------
-- | Configuration of how to display a list of 'Object's of a given
-- 'ObjectType'.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup
       (
         ObjectListSetup(..),
         ObjectListDisplaySetup(..),

         GetMkFooterRowsConstructor,
         ObjectListButtonsSetup(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.WildeUi.WildeValue (AnySVALUE)

import Wilde.ObjectModel.ObjectModelUtils
import qualified Wilde.Media.Presentation as Presentation

import qualified Wilde.ObjectModel.Presentation.FooterRowsConstructor as F


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Specifies the layout and functionality of a list of 'Object's.
data ObjectListSetup otConf atConf dbTable otNative idAtExisting idAtCreate =
  ObjectListSetup
  {
    displaySetup :: ObjectListDisplaySetup otConf atConf dbTable otNative idAtExisting idAtCreate
  , buttonsSetup :: ObjectListButtonsSetup otConf atConf dbTable otNative idAtExisting idAtCreate
   }

-- | Specifies the information to include in a list of 'Object's.
data ObjectListDisplaySetup otConf atConf dbTable otNative idAtExisting idAtCreate =
  ObjectListDisplaySetup
  {
    -- | The 'AttributeType's, and their order, to display in the list.
    displayAts  :: [Any (AttributeType atConf dbTable)]
    -- | Orders the selection from the database (in ascending order) .
  , orderByInDb :: [Any (AttributeType atConf dbTable)]
  , getMkFooterRowsConstructor :: GetMkFooterRowsConstructor otConf atConf dbTable otNative idAtExisting idAtCreate
  }

type GetMkFooterRowsConstructor otConf atConf dbTable otNative idAtExisting idAtCreate =
  Presentation.Monad (F.MkFooterConstructor otConf atConf dbTable otNative idAtExisting idAtCreate)

-- | Specifies the buttons to services for each 'Object', and
-- common buttons for the 'ObjectType' to include in a list of 'Object's.
--
-- 'dbTable' and  'idAtExisting' correspond to the information in the
-- 'ObjectType' that the list displays.
data ObjectListButtonsSetup otConf atConf dbTable otNative idAtExisting idAtCreate =
  ObjectListButtonsSetup
  {
    -- | Buttons to display to the left of each 'Object' in the list.
    -- These buttons are related to the 'Object' on the current row.
    -- May be empty.
    objectButtonsLeft      :: [Presentation.Monad (Object otConf atConf dbTable otNative idAtExisting idAtCreate -> AnySVALUE)]
    -- | Buttons to display to the right of each 'Object' in the list.
    -- These buttons are related to the 'Object' on the current row.
    -- May be empty.
  , objectButtonsRight     :: [Presentation.Monad (Object otConf atConf dbTable otNative idAtExisting idAtCreate -> AnySVALUE)]
    -- | Buttons to display below the list.
    -- These buttons are related to the 'ObjectType' in the list.
  , objectTypeButtonsBelow :: [Presentation.Monad AnySVALUE]
  }
