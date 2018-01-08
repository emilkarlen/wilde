{-# OPTIONS_GHC -i../src  #-}

module Main where

import qualified Wilde.Database.SqlJoin as Sql

import qualified Wilde.Driver.Database.MySQL.DmlExcutor as DmlExecutor


-------------------------------------------------------------------------------
-- - programming project -
-------------------------------------------------------------------------------


-- Database table for programming projects.
--
-- Each value represent a table column.
data ProjectTable
     = ProjectPk
     | ProjectName
     | ProjectLanguageRef

-- Translation of column values to column names.
instance Sql.SQL_IDENTIFIER ProjectTable where
     sqlIdentifier ProjectPk          = "pk"
     sqlIdentifier ProjectName        = "name"
     sqlIdentifier ProjectLanguageRef = "language_ref"

prjEntity :: Sql.Entity ProjectTable
prjEntity = Sql.newEntity "project"

-- Attributes: associates columns with Haskell types
-- These types can be any types.  Usually a type who's
-- values can be constructed from the table columns of the
-- 'Attribute'.  If this is the case, the type should
-- probably uniquely identify the number of columns and
-- the SQL types of these columns.  The reason is that
-- this means that different 'Attribute's with identical
-- value types can be used, e.g., as foreign keys.
prjPkAttr :: Sql.Attribute ProjectTable Int
prjPkAttr = Sql.newAttribute_singleColumn ProjectPk

prjNameAttr :: Sql.Attribute ProjectTable String
prjNameAttr = Sql.newAttribute_singleColumn ProjectName

prjLanguageRefAttr :: Sql.Attribute ProjectTable Int
prjLanguageRefAttr = Sql.newAttribute_singleColumn ProjectLanguageRef


-------------------------------------------------------------------------------
-- - programming language -
-------------------------------------------------------------------------------


-- Database table for programming languages.
--
-- Each value represent a table column.
data LanguageTable
     = LanguagePk
     | LanguageName

-- Translation of column values to column names.
instance Sql.SQL_IDENTIFIER LanguageTable where
     sqlIdentifier LanguagePk          = "pk"
     sqlIdentifier LanguageName        = "name"

lngEntity :: Sql.Entity LanguageTable
lngEntity = Sql.newEntity "language"

-- Attributes: associates columns with Haskell types
-- These types can be any types.  Usually a type who's
-- values can be constructed from the table columns of the
-- 'Attribute'.
lngPkAttr :: Sql.Attribute LanguageTable Int
lngPkAttr = Sql.newAttribute_singleColumn LanguagePk

lngNameAttr :: Sql.Attribute LanguageTable String
lngNameAttr = Sql.newAttribute_singleColumn LanguageName


-------------------------------------------------------------------------------
-- - SELECT -
-------------------------------------------------------------------------------

-- Transform the 'Attributes' to table type "BasedOn ProjectTable",
-- by "including" them.
-- Not that this does not select them in the SQL SELECT statements
-- SELECT clause.
prjPk :: Sql.Attribute (Sql.BasedOn ProjectTable) Int
prjPk   = Sql.includeFromBase prjPkAttr

prjName :: Sql.Attribute (Sql.BasedOn ProjectTable) String
prjName = Sql.includeFromBase prjNameAttr

selectAllProjectsAndTheirLanguageName :: Sql.JoinMonad
                                         ProjectTable
                                         (Sql.SqlSelect (Sql.BasedOn ProjectTable))
selectAllProjectsAndTheirLanguageName =
  do
    languageJoin <- prjLanguageRefAttr `Sql.joinNatural` (lngEntity,lngPkAttr)
    languageJoin2 <- prjLanguageRefAttr `Sql.joinNatural` (lngEntity,lngPkAttr)

    projectJoin1  <- prjPkAttr `Sql.joinNatural` (prjEntity,prjPkAttr)
    projectJoin2  <- prjPkAttr `Sql.joinNatural` (prjEntity,prjPkAttr)

    let lngName   = Sql.includeFromJoined languageJoin lngNameAttr
    let lngName2   = Sql.includeFromJoined languageJoin2 lngNameAttr
    
    let prjName1 = Sql.includeFromJoined projectJoin1 prjNameAttr
    let prjName2 = Sql.includeFromJoined projectJoin2 prjNameAttr
    
    prjNameExprs <- Sql.fieldExprList prjName
    lngNameExprs <- Sql.fieldExprList lngName
    lngNameExprs2 <- Sql.fieldExprList lngName2
    
    prjNameExprs1 <- Sql.fieldExprList prjName1
    prjNameExprs2 <- Sql.fieldExprList prjName2
    
    let select = Sql.simpleSelectWith
                 {
                   Sql.simpleSelectExpressions = prjNameExprs ++ lngNameExprs ++
                                                 lngNameExprs2 ++
                                                 prjNameExprs1 ++ prjNameExprs2
                                                 
                 }
    Sql.simpleSelectStatement select


-------------------------------------------------------------------------------
-- - Render the SQL statement -
-------------------------------------------------------------------------------


-- Renderer for MySQL
renderer = DmlExecutor.mysqlDmlRenderer :: DmlExecutor.DmlRenderer

selectStatement :: Sql.SqlSelect (Sql.BasedOn ProjectTable)
selectStatement = Sql.evalBasedOn prjEntity selectAllProjectsAndTheirLanguageName

dmlStatement :: Sql.SqlDmlStatement (Sql.BasedOn ProjectTable)
dmlStatement = Sql.SqlDmlSelect selectStatement

renderableDmlStatement :: Sql.SqlDmlStatement Sql.SqlIdentifier
renderableDmlStatement = DmlExecutor.toRenderable dmlStatement

main :: IO ()
main = putStrLn $ renderer renderableDmlStatement
