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

-- | Abstract Syntax for the DLL that should be rendered by
-- the DDL renderer for the SQL Back End.
module Wilde.Database.BackEndDdl
       (
         -- * Data Definition Language (DLL)
         
         DdlStatement(..),
         TranslatedDdlStatement,
         TranslatedBackEndTableInfo,
         
         AlterSpecification(..),
         
         -- * Table information
         
         ForeignKeyTarget(..),

         BackEndColumnInfo(..),
         BackEndTableInfo(..),
         BackEndForeignKeyInfo(..),
         
         -- * Comments
         
         PrecededByCommentLines(..),
         withComments,
         withoutComments,
         
         -- * Rendering
         
         DdlRenderer(..),
         
         -- * From SQL
         
         SqlIdentifier,
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC.ColTypes as HDBC

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Database.Sql (SqlIdentifier)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data PrecededByCommentLines a =
  PrecededByCommentLines
  {
    commentLines   :: [String]
  , commentedValue :: a
  }

instance Functor PrecededByCommentLines where
  fmap f x = x { commentedValue = f (commentedValue x) }

-- | Constructs a value that has no comments associated without it.
withoutComments :: a -> PrecededByCommentLines a
withoutComments = PrecededByCommentLines []

withComments :: [String] -> a -> PrecededByCommentLines a
withComments = PrecededByCommentLines

-- | DLL that has been translated from the structures in the Object Model.
-- To get the concrete DLL text, the only thing that remains is rendering
-- by the SQL Back End Renderer.
type TranslatedDdlStatement = DdlStatement BackEndColumnInfo

-- | Corresponds to 'TranslatedDdlStatement'.
type TranslatedBackEndTableInfo = BackEndTableInfo BackEndColumnInfo

-- | Data Definition Language Statements that need to be supported by Wilde
data DdlStatement columnInfo
  = CreateTable (BackEndTableInfo columnInfo)
  | AlterTable  (SqlIdentifier,[AlterSpecification columnInfo])

instance Functor DdlStatement where
  fmap f (CreateTable ct)      = CreateTable (fmap f ct)
  fmap f (AlterTable  (tn,ss)) = AlterTable  (tn,map (fmap f) ss)
  
-- | An "item" of an SQL table that to alter in a ALTER TABLE statement.
data AlterSpecification columnInfo =
  AddForeignKey BackEndForeignKeyInfo
                 
instance Functor AlterSpecification where  
  fmap f (AddForeignKey x) = AddForeignKey x
  
data BackEndColumnInfo =
  BackEndColumnInfo
  {
    colName  :: SqlIdentifier
  , colType  :: HDBC.SqlColDesc
  , colExtra :: [String]
  }
  deriving (Eq,Show,Read)

data BackEndTableInfo columnInfo =
  BackEndTableInfo
  {
    tblName              :: SqlIdentifier
  , tblColumns           :: [columnInfo]
  , tblPrimaryKeyColumns :: NonEmpty.List SqlIdentifier
    -- | Foreign keys: (column in this table, info about destination table)
  , tblForeignKeys       :: [BackEndForeignKeyInfo]
  }
  deriving (Eq,Ord,Show,Read)

instance Functor BackEndTableInfo where
  fmap f ti = ti { tblColumns = map f (tblColumns ti) }

data BackEndForeignKeyInfo =
     BackEndForeignKeyInfo
     {
       fkSrcColumn :: SqlIdentifier
     , fkTarget    :: ForeignKeyTarget
     }
  deriving (Eq,Ord,Show,Read)

data ForeignKeyTarget =
  ForeignKeyTarget
  {
    fkTargetTable  :: SqlIdentifier
  , fkTargetColumn :: SqlIdentifier
  }
  deriving (Eq,Ord,Show,Read)


-------------------------------------------------------------------------------
-- - render -
-------------------------------------------------------------------------------


-- | Constructs DDL for a single database backend (MySQL, PostgreSQL, ...)
data DdlRenderer =
     DdlRenderer
     {
       render :: [PrecededByCommentLines TranslatedDdlStatement] -> String
     }
