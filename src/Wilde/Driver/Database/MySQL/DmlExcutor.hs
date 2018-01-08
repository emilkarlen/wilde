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

-- | DML Executor for MySQL.
module Wilde.Driver.Database.MySQL.DmlExcutor
       (
         module Wilde.Database.Executor,
         
         mysqlDmlRenderer,
         -- mysqlDmlExecutor,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Database.Executor
import Wilde.Database.DmlRendering


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


mysqlDmlRenderer :: DmlRenderer
mysqlDmlRenderer = renderSql

-- mysqlDmlExecutor :: DmlExecutor
-- mysqlDmlExecutor stmt conn params = quickQuery conn sqlString params
--   where
--     sqlString = mysqlDmlRenderer stmt
