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


import           Wilde.WildeUi.StdValueTypes

import           Wilde.Service.ServiceLink

import Wilde.Driver.Application.Cgi.ServiceLink (href)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Renders a 'ServiceLink' as a 'AnySVALUE'.
type ServiceLinkRenderer = ServiceLink -> AnySVALUE


-------------------------------------------------------------------------------
-- | Renders a link to a service.
--
-- The display of the link is an attribute value in the form of a string.
-------------------------------------------------------------------------------
renderServiceLink_string :: WildeStyle -> String -> ServiceLinkRenderer
renderServiceLink_string style display serviceLink =
  let
    wwwLink = linkValue serviceLink (UnquotedStringValue display)
  in
   AnySVALUE $ withNeutralWildeStyle $
   hideStyle $
   withWildeStyle style wwwLink

-------------------------------------------------------------------------------
-- | Renders a link to a service.
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
