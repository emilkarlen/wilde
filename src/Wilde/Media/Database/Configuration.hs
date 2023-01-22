-- | Information about a database driver.
module Wilde.Media.Database.Configuration
       (
         Configuration(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC

import Wilde.Database.DmlRenderer


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Configuation for executing SQL.
data Configuration =
  Configuration
  {
    connectionProvider :: IO ConnWrapper
  , dmlRenderer        :: DmlRenderer
  }
