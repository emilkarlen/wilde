-- | Some components used by Wilde.
module Wilde.WildeUi.WildeComponent
       (
         -- * Table

         TableListComponent(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Render.AbstractTableToHtml

import           Wilde.WildeUi.UiPrimitives
import           Wilde.WildeUi.WildeTable


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - TableListComponent -
-------------------------------------------------------------------------------


-- | A component containing a table list.
newtype TableListComponent = TableListComponent WildeTable

instance COMPONENT TableListComponent where
    componentHtml (TableListComponent wt) = Wilde.Render.AbstractTableToHtml.renderTable wt
