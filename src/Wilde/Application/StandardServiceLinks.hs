-------------------------------------------------------------------------------
-- | Defintion of the \"standard\" services.
--
-- These are services for which the library provides default implementations
-- for.
--
-- NOTE: It would be nice to _not_ provide any info about these services
-- outside of "Wilde.ApplicationConstruction.StandardServices".
-- But for the moment, this is not possible.
--
-- The reason is that the User Interaction Monad's environment must provide
-- a 'StandardServiceLinkRenderer'.
--
-- And the reason for this is: We want to be able to easily configure
-- different 'StandardServiceLinkRenderer' for different
-- Application Drivers - we do _not_ want the renderer to be hard coded
-- in the Object Model.
-------------------------------------------------------------------------------
module Wilde.Application.StandardServiceLinks
       (
        MkObjectTypeServiceLink,
        MkObjectServiceLink,
        CrossRefIdentifier,
        GenericParameter,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Application.StandardServices
import Wilde.Service.ServiceLink (GenericParameter)
import Wilde.WildeUi.WildeValue (AnySVALUE)
import Wilde.Media.GenericStringRep (GenericStringRep)
import Wilde.Media.WildeMedia (CrossRefIdentifier)
import Wilde.WildeUi.WildeStyle (WildeStyling)
import Wilde.WildeUi.StdValueTypes (LinkLabel)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Renders a UI element that is a link to a standard object type service.
type MkObjectTypeServiceLink =
  StandardObjectTypeServiceEnum ->
  WildeStyling LinkLabel ->
  CrossRefIdentifier ->
  [GenericParameter] ->
  AnySVALUE


-- | Renders a UI element that is a link to a standard object service.
type MkObjectServiceLink =
      StandardObjectServiceEnum ->
      WildeStyling LinkLabel ->
      CrossRefIdentifier ->
      GenericStringRep ->
      [GenericParameter] ->
      AnySVALUE
