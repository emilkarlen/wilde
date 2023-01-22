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
