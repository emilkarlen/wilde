-------------------------------------------------------------------------------
-- | Look's up an SQL expression on a given 'ObjectType'
-- in the Custom Environment.
--
-- This expression is supposed to be a boolean expression,
-- so that it can be used to filter 'Object's.
--
-- The variable for the expression is
--
-- _selection.<ObjectType-cross-ref-key>
--
-- Many services looks up this variable, so that filters
-- on 'ObjectType's can be supplied in a uniform way.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.Output.StandardFilterExpression
       (
         elementKey,

         lookupExpression,
         lookupExpression_BasedOn,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad

import qualified Wilde.Database.SqlJoin as Sql

import qualified Wilde.Media.Element as EL
import qualified Wilde.Media.ElementSet as ES
import qualified Wilde.Media.CustomEnvironment as CustomEnvironment

import           Wilde.ObjectModel.ObjectModel

import qualified Wilde.Driver.Application.Cgi.VariableNames as VariableNames (selectExpression)

import qualified Wilde.ApplicationConstruction.UserInteraction.Input.WildeSqlInputer as SqlInputer
import qualified Wilde.ObjectModel.Database as DatabaseClasses


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | ElementKey for the variable for the standard filter for a given
-- 'ObjectType'.
-------------------------------------------------------------------------------
elementKey :: ObjectType otConf atConf dbTable otNative idAtE idAtC
               -> EL.ElementKey
elementKey ot = EL.elementKey
              (EL.elementKeyPrefixFromString VariableNames.selectExpression)
              (otCrossRefKey ot)

-------------------------------------------------------------------------------
-- | Looks up SQL expression on the given 'ObjectType' in the
-- 'CustomEnvironment' in the standard variable.
-------------------------------------------------------------------------------
lookupExpression :: (DatabaseClasses.COLUMN_NAMES atConf
                    ,Sql.SQL_IDENTIFIER dbTable
                    ,CustomEnvironment.MonadWithCustomEnvironmentAndLookup m
                    )
                 => ObjectType otConf atConf dbTable otNative idAtE idAtC
                 -> m (Maybe (Sql.SqlExpr dbTable))
lookupExpression ot =
  CustomEnvironment.inCustomEnvironment $
  ES.mkLookuper lookupParser (elementKey ot)
  where
    lookupParser = ES.singleton_optional >=>
                   ES.nothingIsNothing (SqlInputer.expression_mandatory ot)

-------------------------------------------------------------------------------
-- | As 'lookupExpression' but gives an expression on the \"query\"
-- 'Sql.BasedOn' dbTable.
-------------------------------------------------------------------------------
lookupExpression_BasedOn :: (DatabaseClasses.COLUMN_NAMES atConf
                            ,Sql.SQL_IDENTIFIER dbTable
                            ,CustomEnvironment.MonadWithCustomEnvironmentAndLookup m
                            )
                         => ObjectType otConf atConf dbTable otNative idAtE idAtC
                         -> m (Sql.JoinMonad dbTable (Maybe (Sql.SqlExpr (Sql.BasedOn dbTable))))
lookupExpression_BasedOn ot =
    lookupExpression ot >>= pure . Sql.liftMbExprInMonad
