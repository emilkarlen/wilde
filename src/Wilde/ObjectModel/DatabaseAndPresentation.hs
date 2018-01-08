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

-- | Functionallity required from types that support both
-- database and presentation medias.
module Wilde.ObjectModel.DatabaseAndPresentation
       (
         ATTRIBUTE_TYPE_INFO(..),
         
         AttributeWithPresentationInfoDbInputerInfo(..),
         AttributeWithPresentationInfoDbInputer,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC

import qualified Wilde.Database.SqlJoin as Sql

import Wilde.Media.Database

import qualified Wilde.Media.Presentation as Presentation
import Wilde.ObjectModel.ObjectModelUtils

import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Presentation as Presentation


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


class (Database.COLUMN_NAMES atConf,Presentation.ATTRIBUTE_PRESENTATION atConf) => 
      ATTRIBUTE_TYPE_INFO atConf where
  atDbPresentationInfoGetter :: AttributeType atConf                          dbTable typeForExisting typeForCreate
                             -> AttributeWithPresentationInfoDbInputerInfo dbTable typeForExisting

-------------------------------------------------------------------------------
-- | Lets an 'AttributeType' get extra information from the database
-- for constructing the with-presentation-info variant.
-------------------------------------------------------------------------------
data AttributeWithPresentationInfoDbInputerInfo dbTable e =
  AttributeWithPresentationInfoDbInputerInfo
  (
    Maybe (Sql.JoinMonad dbTable [Sql.SqlExpr (Sql.BasedOn dbTable)],
           AttributeWithPresentationInfoDbInputer e
          )
  )

-------------------------------------------------------------------------------
-- | Function that constructs an 'Attribute' with presentation information
-- from the database.
-- Arguments are:
-- * the 'AttributeType'
--
-- * the 'Attribute's representation value
--
-- * SQL values needed for constructing the presentation value
--
-------------------------------------------------------------------------------
type AttributeWithPresentationInfoDbInputer a =
  a
  -> [SqlValue]
  -> TranslationMonad (Presentation.Monad PresentationOutput)
