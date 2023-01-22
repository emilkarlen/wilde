-------------------------------------------------------------------------------
-- | Generation of SQL statements that select
-- the variant of objects that include presentation information.
--
-- The difference from \"plain\" objects is that these selectes
-- may require joins.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Database.Sql.WithPresentationInfo
       (
         selectAll,
         selectOne,

         -- * ORDER BY Utilities

         orderByNone,
         otDatabaseOrderBy,

         -- * Utilities

         inputInfoAndSelect,

         AttributeTypeDbInputInfo(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Database.SqlJoin as Sql

import qualified Wilde.ObjectModel.Database.JoinUtils as OmDbJ
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Database.Utils as DatabaseUtils
import Wilde.ObjectModel.DatabaseAndPresentation
import Wilde.ObjectModel.ObjectModelUtils

import qualified Wilde.ObjectModel.Database.InputExistingSansPresentationInfo as InputExisting


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Information about how to input an 'Attribute' from SQL data.
-------------------------------------------------------------------------------
data AttributeTypeDbInputInfo atConf dbTable e c =
  AttributeTypeDbInputInfo
  {
    atdbiiAt       :: AttributeType atConf dbTable e c,
    atdbiiPresInfo :: Maybe ([Sql.SqlExpr (Sql.BasedOn dbTable)],
                             AttributeWithPresentationInfoDbInputer e)
  }

-------------------------------------------------------------------------------
-- | Selection of all objects.
-------------------------------------------------------------------------------
selectAll :: (Database.DATABASE_TABLE otConf
             ,ATTRIBUTE_TYPE_INFO atConf
             ,InputExisting.COLUMN_NAMES atConf)
          => ObjectType otConf atConf dbTable otN idAE idAC
          -> [Any (AttributeType atConf dbTable)]
          -- ^ ORDER BY columns
          -> Sql.SqlSelect (Sql.BasedOn dbTable)
selectAll ot orderByInDb =
  snd $
  Sql.evalBasedOn (OmDbJ.newSqlEntity ot) $
  do
    orderByExprs <- otDatabaseOrderBy orderByInDb
    getInputInfosAndSelect ot Nothing orderByExprs

-------------------------------------------------------------------------------
-- | Selection of a single object.
--
-- Parameters of the statement are the ID-attribute of type idAtExisting.
-------------------------------------------------------------------------------
selectOne :: (Database.DATABASE_TABLE otConf
             ,ATTRIBUTE_TYPE_INFO atConf
             ,InputExisting.COLUMN_NAMES atConf)
          => ObjectType otConf atConf dbTable otN idAE idAC
          -> Sql.SqlSelect (Sql.BasedOn dbTable)
selectOne ot = snd $
               Sql.evalBasedOn (OmDbJ.newSqlEntity ot) $
               getInputInfosAndSelect ot justWhereExpr []
  where
    justWhereExpr = Just $ whereExprForIdAt ot
    whereExprForIdAt :: InputExisting.COLUMN_NAMES atConf
                     => ObjectType otConf atConf dbTable otN idAE idAC
                     -> Sql.SqlExpr (Sql.BasedOn dbTable)
    whereExprForIdAt = Sql.liftExprFromBase . DatabaseUtils.otIdAtEqPosParamExpr

-------------------------------------------------------------------------------
getOtAttributeTypeDbInputInfo :: ATTRIBUTE_TYPE_INFO atConf
                              => ObjectType otConf atConf dbTable otN idAE idAC
                              -> Sql.JoinMonad dbTable (AttributeTypeDbInputInfo atConf dbTable idAE idAC,
                                                        [Any (AttributeTypeDbInputInfo atConf dbTable)])
getOtAttributeTypeDbInputInfo ot =
  do
    idAtInfo     <- getAttributeTypeDbInputInfo idAt
    nonIdAtInfos <- mapM (\(Any at) -> fmap Any (getAttributeTypeDbInputInfo at)) nonIdAts
    return $ (idAtInfo,nonIdAtInfos)
  where
    idAt     = otIdAttributeType ot
    nonIdAts = otNonIdAttributeTypes ot

-------------------------------------------------------------------------------
-- | Helper for 'readObjectWithPresentationInfo'.
-------------------------------------------------------------------------------
getAttributeTypeDbInputInfo :: ATTRIBUTE_TYPE_INFO atConf
                            => AttributeType atConf dbTable e c
                            -> Sql.JoinMonad dbTable (AttributeTypeDbInputInfo atConf dbTable e c)
getAttributeTypeDbInputInfo at =
  case atDbPresentationInfoGetter at of
    (AttributeWithPresentationInfoDbInputerInfo Nothing) -> return $ AttributeTypeDbInputInfo at Nothing
    (AttributeWithPresentationInfoDbInputerInfo (Just (m,mkA))) ->
     do
       presInfoExprs <- m
       return $ AttributeTypeDbInputInfo at (Just (presInfoExprs,mkA))

-------------------------------------------------------------------------------
-- | Inputer and generic select.
-------------------------------------------------------------------------------
inputInfoAndSelect :: (Database.DATABASE_TABLE otConf
                       ,ATTRIBUTE_TYPE_INFO atConf
                       )
                   => ObjectType otConf atConf dbTable otN idAE idAC
                   -> Sql.JoinMonad dbTable (Maybe (Sql.SqlExpr (Sql.BasedOn dbTable)))
                   -- ^ WHERE expression
                   -> Sql.JoinMonad dbTable [Sql.SqlExpr (Sql.BasedOn dbTable)]
                   -- ^ ORDER BY expressions
                   -> ((AttributeTypeDbInputInfo atConf dbTable idAE idAC,
                        [Any (AttributeTypeDbInputInfo atConf dbTable)]),
                       Sql.SqlSelect (Sql.BasedOn dbTable))
inputInfoAndSelect ot getMbWhereExpr getOrderByExprs =
  Sql.evalBasedOn (OmDbJ.newSqlEntity ot) getInputInfosAndSelect'
  where
    getInputInfosAndSelect' =
      do
        mbWhereExpr  <- getMbWhereExpr
        orderByExprs <- getOrderByExprs
        getInputInfosAndSelect ot mbWhereExpr orderByExprs

-------------------------------------------------------------------------------
getInputInfosAndSelect :: (ATTRIBUTE_TYPE_INFO atConf
                          ,InputExisting.COLUMN_NAMES atConf)
                       => ObjectType otConf atConf dbTable otN idAE idAC
                       -> Maybe (Sql.SqlExpr (Sql.BasedOn dbTable))
                       -- ^ WHERE expression
                       -> [Sql.SqlExpr (Sql.BasedOn dbTable)]
                       -- ^ ORDER BY expressions
                       -> Sql.JoinMonad dbTable ((AttributeTypeDbInputInfo atConf dbTable idAE idAC,
                                                  [Any (AttributeTypeDbInputInfo atConf dbTable)]),
                                                 Sql.SqlSelect (Sql.BasedOn dbTable))
getInputInfosAndSelect ot@(ObjectType {}) mbWhereExpr orderByExprs =
  do
    inputInfo  <- getOtAttributeTypeDbInputInfo ot
    selectStmt <- selectStatement inputInfo mbWhereExpr orderByExprs
    return (inputInfo,selectStmt)

-------------------------------------------------------------------------------
selectStatement :: InputExisting.COLUMN_NAMES atConf
                => (AttributeTypeDbInputInfo atConf dbTable idAE idAC,
                    [Any (AttributeTypeDbInputInfo atConf dbTable)])
                -> Maybe (Sql.SqlExpr (Sql.BasedOn dbTable))
                -> [Sql.SqlExpr (Sql.BasedOn dbTable)]
                -> Sql.JoinMonad dbTable (Sql.SqlSelect (Sql.BasedOn dbTable))
selectStatement (idAtInfo@(AttributeTypeDbInputInfo { atdbiiAt = at@(AttributeType {})}),
                 nonIdAtInfos)
  mbWhereExpr
  orderByExprs =
  do
    idAtExprs        <- atExprs idAtInfo
    nonIdAtExprsList <- mapM (anyValueApply atExprs) nonIdAtInfos
    let atExprs       = idAtExprs ++ concat nonIdAtExprsList
    Sql.selectStatement atExprs mbWhereExpr orderByExprs

-------------------------------------------------------------------------------
atExprs :: InputExisting.COLUMN_NAMES atConf
        => AttributeTypeDbInputInfo atConf dbTable e c
        -> Sql.JoinMonad dbTable [Sql.SqlExpr (Sql.BasedOn dbTable)]
atExprs (AttributeTypeDbInputInfo at@(AttributeType {}) mbPresInfo) =
  do
    baseAtExprs    <- OmDbJ.atColumnExprList at
    let presAtExprs = maybe [] fst mbPresInfo
    return $ baseAtExprs ++ presAtExprs


-------------------------------------------------------------------------------
-- - ORDER BY utilities -
-------------------------------------------------------------------------------


-- | Gives the SQL for the \"ORDER BY-expressions\", for ordering according to an
-- 'ObjectType's standard order.
otDatabaseOrderBy :: InputExisting.COLUMN_NAMES atConf
                  => [Any (AttributeType atConf dbTable)]
                  -> Sql.JoinMonad      dbTable [Sql.SqlExpr (Sql.BasedOn dbTable)]
otDatabaseOrderBy orderByAts = fmap concat getOrderByAtsExprs
  where
    getOrderByAtsExprs = mapMAttributeTypeAnyValue OmDbJ.atColumnExprList orderByAts

-- | \"ORDER BY-expressions\" for no ordering.
orderByNone :: Sql.JoinMonad dbTable [Sql.SqlExpr (Sql.BasedOn dbTable)]
orderByNone = return []
