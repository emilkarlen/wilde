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
