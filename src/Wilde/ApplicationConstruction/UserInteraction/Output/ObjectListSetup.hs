{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------
-- | Configuration of how to display a list of 'Object's of a given
-- 'ObjectType'.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup
       (
         ObjectListSetup(..),
         ObjectListDisplaySetup(..),

         GetFooterRowsConstructor,
         ObjectListButtonsSetup(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.WildeValue (AnySVALUE)

import Wilde.ObjectModel.ObjectModelUtils
import qualified Wilde.Media.Presentation as Presentation

import qualified Wilde.ObjectModel.Presentation as OmPres


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Specifies the layout and functionality of a list of 'Object's.
data ObjectListSetup otConf atConf dbTable otNative idAtExisting idAtCreate =
  ObjectListSetup
  {
    displaySetup :: ObjectListDisplaySetup otConf atConf dbTable otNative idAtExisting idAtCreate
  , buttonsSetup :: ObjectListButtonsSetup                             idAtExisting
   }

-- | Specifies the information to include in a list of 'Object's.
data ObjectListDisplaySetup otConf atConf dbTable otNative idAtExisting idAtCreate =
  forall acc .
  ObjectListDisplaySetup
  {
    -- | The 'AttributeType's, and their order, to display in the list.
    displayAts  :: [Any (AttributeType atConf dbTable)]
    -- | Orders the selection from the database (in ascending order) .
  , orderByInDb :: [Any (AttributeType atConf dbTable)]
  , getFooterRowsConstructor :: GetFooterRowsConstructor acc otConf atConf dbTable otNative idAtExisting idAtCreate
  }

-- | Gives an "constructor" of footer rows, if there should be any.
-- (There are two ways to say that there should be no footer rows: One is
-- to let this method give Nothing; the other is to give
-- an empty list of footer rows.)
type GetFooterRowsConstructor acc otConf atConf dbTable otNative idAtExisting idAtCreate =
  Presentation.Monad (Maybe (OmPres.FooterRowsConstructor acc otConf atConf dbTable otNative idAtExisting idAtCreate))

-- | Specifies the buttons to services for each 'Object', and
-- common buttons for the 'ObjectType' to include in a list of 'Object's.
--
-- 'dbTable' and  'idAtExisting' correspond to the information in the
-- 'ObjectType' that the list displays.
data ObjectListButtonsSetup idAtExisting =
  ObjectListButtonsSetup
  {
    -- | Buttons to display to the left of each 'Object' in the list.
    -- These buttons are related to the 'Object' on the current row.
    -- May be empty.
    objectButtonsLeft      :: [Presentation.Monad (idAtExisting -> AnySVALUE)]
    -- | Buttons to display to the right of each 'Object' in the list.
    -- These buttons are related to the 'Object' on the current row.
    -- May be empty.
  , objectButtonsRight     :: [Presentation.Monad (idAtExisting -> AnySVALUE)]
    -- | Buttons to display below the list.
    -- These buttons are related to the 'ObjectType' in the list.
  , objectTypeButtonsBelow :: [Presentation.Monad AnySVALUE]
  }


