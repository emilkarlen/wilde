-- | DML Executor for MySQL.
module Wilde.Driver.Database.MySQL.DmlExcutor
       (
         DmlRenderer,
         mysqlDmlRenderer,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Database.DmlRendering
import Wilde.Database.DmlRenderer


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


mysqlDmlRenderer :: DmlRenderer
mysqlDmlRenderer = renderSql
