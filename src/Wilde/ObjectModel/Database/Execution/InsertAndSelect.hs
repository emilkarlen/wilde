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

-------------------------------------------------------------------------------
-- | Utilities for insertion followed immediately by selection
-- of a single object.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Database.Execution.InsertAndSelect
       (
         insertAndSelectOne_sansPresInfo,
         insertAndSelectOne_withPresInfo,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.ObjectModel.ObjectModel
    ( ObjectForCreate(ofcType), Object )

import Wilde.Media.Database

import qualified Wilde.ObjectModel.Database.Output as Output

import qualified Wilde.ObjectModel.Database.InputExistingSansPresentationInfo as InputExisting
import qualified Wilde.ObjectModel.Database.Execution.SelectWithPresentationInfo as SelectWithPres
import qualified Wilde.ObjectModel.Database.Execution.Insert as Insert
import qualified Wilde.ObjectModel.Database.Execution.SelectSansPresentationInfo as SelectPlain
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import qualified Wilde.Media.Database.Monad as DbConn


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts one object and reads it from the database.
--
-- The object is read WITH presentation information.
-------------------------------------------------------------------------------
insertAndSelectOne_withPresInfo :: (Database.OBJECT_TYPE_INSERT otConf
                                   ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                                   ,Output.OUTPUT_FOR_CREATE atConf
                                   ,Output.OUTPUT_FOR_EXISTING atConf
                                   ,InputExisting.INPUT_FOR_EXISTING atConf
                                   ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                                   )
                                => ObjectForCreate otConf atConf dbTable otNative idAtExisting idAtCreate
                                -> DbConn.Monad (Object otConf atConf dbTable otNative idAtExisting idAtCreate)
insertAndSelectOne_withPresInfo oc =
  do
    idAtValue <- Insert.insertOneGetId oc
    mbObject <- SelectWithPres.inputOne (ofcType oc) idAtValue
    let errMsg = "Just inserted an object, but did not get one when trying to read it from the DB"
    maybe (DbConn.throwErr (DbUnclassifiedError errMsg)) return mbObject

-------------------------------------------------------------------------------
-- | Inserts one object and reads it from the database.
--
-- The object is read WITHOUT presentation information.
-------------------------------------------------------------------------------
insertAndSelectOne_sansPresInfo :: (Database.OBJECT_TYPE_INSERT otConf
                                   ,Database.COLUMN_NAMES atConf
                                   ,Output.OUTPUT_FOR_CREATE atConf
                                   ,Output.OUTPUT_FOR_EXISTING atConf
                                   ,InputExisting.INPUT_FOR_EXISTING atConf)
                                => ObjectForCreate otConf atConf dbTable otNative idAtExisting idAtCreate
                                -> DbConn.Monad (Object otConf atConf dbTable otNative idAtExisting idAtCreate)
insertAndSelectOne_sansPresInfo oc =
  do
    idAtValue <- Insert.insertOneGetId oc
    mbObject <- SelectPlain.selectOne (ofcType oc) idAtValue
    let errMsg = "Just inserted an object, but did not get one when trying to read it from the DB"
    maybe (DbConn.throwErr (DbUnclassifiedError errMsg)) return mbObject
