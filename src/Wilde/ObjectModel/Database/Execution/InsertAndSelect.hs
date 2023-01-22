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
    maybe (DbConn.throwErr (DbUnclassifiedError errMsg)) pure mbObject

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
    maybe (DbConn.throwErr (DbUnclassifiedError errMsg)) pure mbObject
