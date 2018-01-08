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


import Wilde.Render.ServiceLink

import Wilde.ApplicationConstruction.StandardServices


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
renderLink = renderServiceLink_string
