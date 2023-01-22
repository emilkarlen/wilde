{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------
-- | Commands related to the database.
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.Commands.Database
       (
         printSql,
         dbSelectAll_sqlRecord,
         dbSelectAll_tupleString,
         printCreateTables,
         printObjectTypeInfo,
         printAttributeTypes,
       )
       where



-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.List

import System.IO

import Wilde.Utils.Utils as Utils
import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Database.Sql
import qualified Wilde.ApplicationTool.DbExecution as SqlExec

import qualified Wilde.Media.ElementSet as ES (empty,ElementSet)

import qualified Wilde.ObjectModel.Database as DatabaseClasses

import Wilde.ApplicationTool.Commands.SqlDdl

import qualified Wilde.ObjectModel.Database.Sql.SansPresentationInfo as SqlSansPres
import qualified Wilde.ObjectModel.Database.Sql.WithPresentationInfo as SqlWithPres

import Wilde.ObjectModel.ObjectModelUtils

import Wilde.Media.Database

import qualified Wilde.ObjectModel.Database.Sql.WithPresentationInfo as SqlWithPresentationInfo

import Wilde.ApplicationTool.FlagsAndOptions
import qualified Wilde.ApplicationTool.FlagLookup.ForObjectType as FlagLookupForObjectType
import qualified Wilde.ApplicationTool.FlagLookup.ForMisc as FlagLookup

import Wilde.ApplicationTool.ApplicationModel
import Wilde.ApplicationTool.Command

import Wilde.ApplicationTool.Commands.Utils
import qualified Wilde.ApplicationTool.Commands.DatabaseUtils as CommandDatabaseUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


printSql :: OBJECT_TYPES_WITH_DATABASE_IO_INFO om
         => CommandWithParsedArgs om
printSql (CommandEnv { objectModel = om,dmlRenderer = theDmlRenderer }) (flags,arguments) =
  do
    objectTypes <- FlagLookupForObjectType.typesWithDatabaseIo_oneOrMore om flags
    sqlStmt     <- FlagLookup.flagSqlStatement_mandatory flags
    mapM_ (printSqlForObjectType theDmlRenderer sqlStmt) (NonEmpty.toList objectTypes)

printSqlForObjectType :: SqlExec.DmlRenderer
                      -> SqlStatement
                      -> AnyOWithDatabaseIo ObjectType
                      -> IO ()
printSqlForObjectType dmlRenderer sqlAction (AnyOWithDatabaseIo ot@(ObjectType {})) =
  maybe (hPutStrLn stderr "Not implemented") putStrLn mbSql
  where
    mbSql = case sqlAction of
      SelectAllPlain -> Just $ renderSql $ toSI $ SqlDmlSelect $ SqlSansPres.selectAll ot []
      SelectOnePlain -> Just $ renderSql $ toSI $ SqlDmlSelect $ SqlSansPres.selectOne ot
      SelectAllPres  -> Just $ renderSql $ toSI $ SqlDmlSelect $ SqlWithPres.selectAll ot []
      SelectOnePres  -> Just $ renderSql $ toSI $ SqlDmlSelect $ SqlWithPres.selectOne ot
      InsertOne      -> Just $ renderSql $ toSI $ SqlDmlInsert $ SqlSansPres.insertOne ot
      UpdateOne      -> Just $
                        maybe
                        "no updatable attributes"
                        (renderSql . toSI . SqlDmlUpdate . (SqlSansPres.updateOne ot))
                        (NonEmpty.fromList . otNonIdAttributeTypes $ ot)
      DeleteOne      -> Just $ renderSql $ toSI $ SqlDmlDelete $ SqlSansPres.deleteOne ot
    renderSql = dmlRenderer

toSI :: SQL_IDENTIFIER col => SqlDmlStatement col -> SqlDmlStatement SqlIdentifier
toSI = fmap sqlIdentifier

dbSelectAll_sqlRecord :: OBJECT_TYPES_WITH_DATABASE_IO_INFO om
                      => CommandWithParsedArgs om
dbSelectAll_sqlRecord env@(CommandEnv { objectModel = om }) (flags,arguments) =
  do
    ots  <- FlagLookupForObjectType.typesWithDatabaseIo_oneOrMore om flags
    car  <- getCar env
    mapM_ (dbSelectAll_forObjectType_sqlRecord car) (NonEmpty.toList ots)

dbSelectAll_forObjectType_sqlRecord :: SqlExec.ConnectionAndRenderer
                                    -> AnyOWithDatabaseIo ObjectType
                                    -> IO ()
dbSelectAll_forObjectType_sqlRecord car (AnyOWithDatabaseIo ot@(ObjectType {})) =
  do
    printObjectTypeHeader ot
    let sql = SqlWithPresentationInfo.selectAll ot (otAttributeTypes ot)
    rows   <- SqlExec.quickSelect car sql []
    mapM_ putStrLn $ table ' ' " " $ map (map show) rows

dbSelectAll_tupleString :: OBJECT_TYPES_WITH_DATABASE_IO_INFO om
                        => CommandWithParsedArgs om
dbSelectAll_tupleString env@(CommandEnv { objectModel = om }) (flags,arguments) =
  do
    ots <- FlagLookupForObjectType.typesWithDatabaseIo_oneOrMore om flags
    car     <- getCar env
    mapM_ (dbSelectAll_forObjectType_tupleString car) (NonEmpty.toList ots)

dbSelectAll_forObjectType_tupleString :: SqlExec.ConnectionAndRenderer
                                      -> AnyOWithDatabaseIo ObjectType
                                      -> IO ()
dbSelectAll_forObjectType_tupleString car (AnyOWithDatabaseIo ot) =
  do
    printObjectTypeHeader ot
    objects <- CommandDatabaseUtils.getObjectsWithConn car ot
    mapM_ putStrLn $ map tupleString objects

-- What does this command do?????
--
-- (operates on AttributeType or Attribute??)
printAttributeTypes :: OBJECT_TYPES_WITH_DATABASE_IO_INFO om
                    => CommandWithParsedArgs om
printAttributeTypes env@(CommandEnv { objectModel = om }) (flags,arguments) =
  do
    AnyOWithDatabaseIo objectType <- FlagLookupForObjectType.typesWithDatabaseIo_single om flags
    objects <- CommandDatabaseUtils.getObjects env objectType
    let attributeTable = map oAllAttributesAnyValue objects
    mapM_ putStrLn $ stdTable $ map (map atId) attributeTable

  where
    atId :: Any (Attribute atConf dbTable) -> String
    atId = anyValueApply (atCrossRefKey . attrType)

printObjectTypeInfo :: OBJECT_TYPES_WITH_DATABASE_IO_INFO om
                    => CommandWithParsedArgs om
printObjectTypeInfo (CommandEnv { objectModel = om }) (flags,arguments) =
  do
    objectTypes <- FlagLookupForObjectType.typesWithDatabaseIo_oneOrMore om flags
    mapM_ printObjectType (NonEmpty.toList objectTypes)

printObjectType :: AnyOWithDatabaseIo ObjectType -> IO ()
printObjectType (AnyOWithDatabaseIo ot) = mapM_ putStrLn $ Utils.table ' ' "  " table
  where
    table = map printAttributeType $ otAttributeTypes ot :: [[String]]

printAttributeType :: DatabaseClasses.COLUMN_NAMES atConf
               => Any (AttributeType atConf dbTable)
               -> [String]
printAttributeType (Any at@(AttributeType
                            { atCrossRefKey = crossRefKey }
                           )) =
   [crossRefKey,typeForExistingString,concat $ intersperse "," dbColInfos]
  where
    typeForExistingString = typeString at undefined
    dbCols                = NonEmpty.toList $ DatabaseClasses.atColumns at
    dbColInfos            = map dbColInfo dbCols
    dbColInfo :: SQL_IDENTIFIER dbTable => DatabaseColumn dbTable -> String
    dbColInfo (DatabaseColumn field) = fill ' ' 20 $ sqlIdentifier field

typeString :: AttributeType atConf dbTable typeForExisting typeForCreate
           -> typeForExisting
           -> String
typeString at@(AttributeType {}) neverTouched = show $ typeOf neverTouched

customEnvironment :: ES.ElementSet
customEnvironment = ES.empty
