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
-- | Utilities related to 'Command'.
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.Commands.Utils
       (
         stdTable,
         
         printObjectTypeHeader,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Utils.Utils

import Wilde.ObjectModel.ObjectModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Standard formatting of a table.
stdTable = table ' ' "  "

-- | Prints a header for an 'ObjectType'.
printObjectTypeHeader :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                      -> IO ()
printObjectTypeHeader ot = mapM_ putStrLn lines
  where
    lines =
      [
        separator
      , otCrossRefKey ot
      , separator
      ]
    separator = replicate 40 '-'
