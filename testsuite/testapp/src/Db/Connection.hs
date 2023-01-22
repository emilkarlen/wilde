module Db.Connection
       (
         theDbConfiguration,
       )

       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.Database.Configuration as DbConf

import qualified Db.MariaDb as Db


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theDbConfiguration :: DbConf.Configuration
theDbConfiguration = Db.newConf "wilde_test" "dml"
