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
      Nothing -> pure Nothing
      (Just (TheSql sql)) -> parse sql
      _                   -> implError "TheSql"
  where
    parse :: String -> IO (Maybe SqlStatement)
    parse s =
      case Utils.readCompletelyAndUnambigously s of
        Nothing -> msgFail $ "Sql statement must be one of " ++ show sqlStatement_all
        Just x  -> pure (Just x)

flagSqlStatement_mandatory :: [Flag] -> IO SqlStatement
flagSqlStatement_mandatory flags = mkMandatoryIO "SQL-STMT" $ flagSqlStatement flags
