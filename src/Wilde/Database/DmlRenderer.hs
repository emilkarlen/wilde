module Wilde.Database.DmlRenderer where

import Wilde.Database.Sql (SqlDmlStatement, SqlIdentifier, SQL_IDENTIFIER(..))

-- | Rendering of DML SQL for a database driver.
type DmlRenderer = SqlDmlStatement SqlIdentifier -> String

-- | Prepares a structure for rendering.
toRenderable :: (Functor t,SQL_IDENTIFIER a)
             => t a
             -> t SqlIdentifier
toRenderable = fmap sqlIdentifier
