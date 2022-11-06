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
        ObjectTypeServiceLinkRenderer,
        ObjectServiceLinkRenderer,
        CrossRefIdentifier,
        GenericParameter,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Application.StandardServices
import Wilde.Application.ServiceLink (GenericParameter)
import Wilde.Media.WildeValue (AnySVALUE)
import Wilde.Media.GenericStringRep (GenericStringRep)
import Wilde.Media.WildeMedia (CrossRefIdentifier)
import Wilde.Media.WildeStyleType (WildeStyling)
import Wilde.WildeUi.StdValueTypes (LinkLabel)

-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------
--  , ssObjectTypeName :: Maybe CrossRefIdentifier
--  , ssObjectIdentity :: Maybe GenericStringRep

-- | Renders a UI element that is a link to a standard object type service.
type ObjectTypeServiceLinkRenderer =
  StandardObjectTypeServiceEnum ->
  WildeStyling LinkLabel ->
  CrossRefIdentifier ->
  [GenericParameter] ->
  AnySVALUE
      

-- | Renders a UI element that is a link to a standard object service.
type ObjectServiceLinkRenderer =
      StandardObjectServiceEnum ->
      WildeStyling LinkLabel ->
      CrossRefIdentifier ->
      GenericStringRep ->
      [GenericParameter] ->
      AnySVALUE
