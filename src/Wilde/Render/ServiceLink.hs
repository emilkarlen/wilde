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

module Wilde.Render.ServiceLink
       (
         ServiceLinkRenderer,

         href,

         renderServiceLink_string,
         renderServiceLink_svalue,
         renderServiceLink_svalueWithConfirm,

         -- * Re-exporting
         
         ServiceLink(..),
         AnySVALUE(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------



import           Wilde.Media.WildeStyle as WS

import           Wilde.WildeUi.StdValueTypes

import           Wilde.Application.ServiceLink

import Wilde.Driver.Application.Cgi.ServiceLink (href)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Renders a 'ServiceLink' as a 'AnySVALUE'.
type ServiceLinkRenderer = ServiceLink -> AnySVALUE


-------------------------------------------------------------------------------
-- | Renders a link to a service as CGI invokation.
--
-- The display of the link is an attribute value in the form of a string.
-------------------------------------------------------------------------------
renderServiceLink_string :: String -> ServiceLinkRenderer
renderServiceLink_string display serviceLink =
  let
    wwwLink = linkValue serviceLink (UnquotedStringValue display)
  in
   AnySVALUE $ withNeutralWildeStyle $
   hideStyle $
   withWildeStyle WS.referenceAttributeStyle wwwLink

-------------------------------------------------------------------------------
-- | Renders a link to a service as CGI invokation.
--
-- The display of the link is a \"button\" (whatever this means!) that does not
-- need any further formating.
-------------------------------------------------------------------------------
renderServiceLink_svalue :: AnySVALUE -> ServiceLinkRenderer
renderServiceLink_svalue display serviceLink =
  let
    wwwLink = linkValue serviceLink display
  in
   AnySVALUE wwwLink

-- | A variant of 'renderServiceLink_svalue' that requires that the user
-- must confirm that he/she wants to execute the operation associated with the
-- button.
renderServiceLink_svalueWithConfirm :: String -> AnySVALUE -> ServiceLinkRenderer
renderServiceLink_svalueWithConfirm confirmationMessage display serviceLink =
  let
    wwwLink = linkValueWithConfirm confirmationMessage serviceLink display
  in
   AnySVALUE wwwLink


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


linkValue :: SVALUE display => ServiceLink -> display -> WwwLinkValue display
linkValue serviceLink display = wwwLinkValue (href serviceLink) display

linkValueWithConfirm :: SVALUE display
                     => String       -- ^ Confimation message
                     -> ServiceLink  -- ^ Target
                     -> display      -- ^ Link visualization
                     -> WwwLinkValue display
linkValueWithConfirm confirmationMessage serviceLink display =
  wwwLinkValueWithOnClick javaScriptPgm (href serviceLink) display
  where
    -- TODO Fix possible quotes (singlet-quotes only?) inside confirmationMessage
    javaScriptPgm = "return confirm('" ++ confirmationMessage ++ "')"
