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
