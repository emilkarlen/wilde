-- | Commonly used components.
module Wilde.GenericUi.Components
       (
         module Wilde.GenericUi.Component,

         ValueComponent,
         valueComponent,

         anyValueComponent,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.GenericUi.Component
import Wilde.GenericUi.Value


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data ValueComponent a = ValueComponent a

-- | Makes a component from a VALUE'.
valueComponent :: VALUE a => a -> ValueComponent a
valueComponent x = ValueComponent x

anyValueComponent :: VALUE a => a -> AnyCOMPONENT
anyValueComponent = anyComponent . valueComponent

instance VALUE a => COMPONENT (ValueComponent a) where
    componentHtml (ValueComponent x) = valueHtml x
