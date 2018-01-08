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

{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------
-- | Lookup from CLI-flags
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.FlagLookup.ForMisc
       (
         -- * SQL statement
         
         flagSqlStatement,
         flagSqlStatement_mandatory,

       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.Utils as Utils

import Wilde.ApplicationTool.FlagLookup.Utils
import Wilde.ApplicationTool.FlagsAndOptions as FlagsAndOptions


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


flagSqlStatement :: [Flag] -> IO (Maybe SqlStatement)
flagSqlStatement flags =
    case flagOptional flagIsSql flags of
      Nothing -> return Nothing
      (Just (TheSql sql)) -> parse sql
      _                   -> implError "TheSql"
  where
    parse :: String -> IO (Maybe SqlStatement)
    parse s =
      case Utils.readCompletelyAndUnambigously s of
        Nothing -> msgFail $ "Sql statement must be one of " ++ show sqlStatement_all
        Just x  -> return (Just x)

flagSqlStatement_mandatory :: [Flag] -> IO SqlStatement
flagSqlStatement_mandatory flags = mkMandatoryIO "SQL-STMT" $ flagSqlStatement flags
