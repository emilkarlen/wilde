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

-- | A type for components that have some kind of dependence on an 'Object'.
--
-- Example of an dependence is having an 'Attribute' that is a reference
-- to the 'Object'.
--
-- Example of a dependent component is a component that lists all 'Object's 
-- (of some 'ObjectType') that has an 'Attribute' that references the
-- target 'Object'.
module Wilde.ApplicationConstruction.UserInteraction.Output.ObjectDependentComponent
       (
         ObjectDependentComponent(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.Presentation as Presentation

import Wilde.ObjectModel.ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | A \"component\" that depends on an 'Object' of some given type.
--
-- Most common example is an 'ObjectType' that has an 'AttributeType' that
-- is a reference to some other 'ObjectType' (or the same 'ObjectType').
type ObjectDependentComponent otConf atConf dbTable otNative idAtExisting idAtCreate =
                       Object otConf atConf dbTable otNative idAtExisting idAtCreate
                       -> Presentation.Monad (Maybe AnyCOMPONENT)
