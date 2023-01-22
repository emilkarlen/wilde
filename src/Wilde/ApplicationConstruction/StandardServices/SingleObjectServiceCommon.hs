-------------------------------------------------------------------------------
-- | Parts that are common to standard services that operate on a single
-- object.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.StandardServices.SingleObjectServiceCommon
       (
         theObjectName,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.UserInteraction


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | The name of the object that the services operate on.
theObjectName :: ObjectName
theObjectName = ["object"]
