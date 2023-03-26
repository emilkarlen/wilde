-------------------------------------------------------------------------------
-- | A simple 'StandardServiceLinkRenderer'.
--
-- This renderer does not require any external resources
-- (image files, e.g.).
-------------------------------------------------------------------------------
module Wilde.Driver.UserInteraction.StandardServiceLinkRenderer
       (
         renderer,

         LABEL_OF(..),

         -- * Re-exporting

         StandardServiceLinkRenderer,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.WildeUi.WildeStyle as WS
import           Wilde.Render.ServiceLink

import           Wilde.ApplicationConstruction.StandardServices


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


renderer :: StandardServiceLinkRenderer
renderer serviceEnum = renderLink (labelOf serviceEnum)

-- | Class for overloading 'labelOf'.
class LABEL_OF a where
  labelOf :: a -> String

instance LABEL_OF StandardServiceEnum where
  labelOf (StandardObjectTypeService enum) = labelOf enum
  labelOf (StandardObjectService     enum) = labelOf enum

instance LABEL_OF StandardObjectTypeServiceEnum where
  labelOf CreateOne = "[*]"
  labelOf x         = "[" ++ show x ++ "]"

instance LABEL_OF StandardObjectServiceEnum where
  labelOf enum = case enum of
    CreateOneFrom -> "[**]"
    UpdateOne     -> "[~]"
    DeleteOne     -> "[-]"
    ShowOne       -> ">"

-- TODO Använd snyggar unicode tecken för knapparna.

-------------------------------------------------------------------------------
-- | Renders a link to a service as CGI invokation.
--
-- The display of the link is a \"button\" (whatever this means!) that does not
-- need any further formating.
-------------------------------------------------------------------------------
renderLink :: String -> ServiceLink -> AnySVALUE
renderLink = renderServiceLink_string WS.textButtonStyle
