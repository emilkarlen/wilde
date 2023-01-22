-------------------------------------------------------------------------------
-- | Inforamtion that the application driver has to deliver to the
-- application driver.
-------------------------------------------------------------------------------
module Wilde.Application.ApplicationInput
       (
         Input(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.ElementSet


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Input to the application.
data Input =
  Input
  {
    -- | Contains both information that determines which service to execute,
    -- and information input by the user (User Interation Input).
    inputMedia        :: ElementSet
  , customEnvironment :: ElementSet
  }
  deriving (Eq,Show)
