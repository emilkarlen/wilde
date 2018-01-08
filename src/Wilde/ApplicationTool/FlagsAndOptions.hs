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
-- | Definition of the CLI flags and options of the tool.
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.FlagsAndOptions
       (
         -- * Flags
         
         SqlStatement(..),
         sqlStatement_all,
         allEnums,
         Flag(..),
         flagIsObjectType,
         flagIsSql,
         
         -- * Options
         
         options,
         
         optionNameObjectType,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import System.Console.GetOpt


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data SqlStatement = SelectAllPlain
                  | SelectOnePlain
                  | SelectAllPres
                  | SelectOnePres
                  | InsertOne
                  | UpdateOne
                  | DeleteOne
                  deriving (Show,Read,Bounded,Enum)

sqlStatement_all :: [SqlStatement]
sqlStatement_all = allEnums

allEnums :: (Bounded a,Enum a) => [a]
allEnums = [minBound .. maxBound]

data Flag = TheObjectType String
          | TheSql String
          | Version
            deriving Show

-- | Predicate for the flag for Object Type.
flagIsObjectType :: Flag -> Bool
flagIsObjectType (TheObjectType _) = True
flagIsObjectType _                 = False

-- | Predicate for the flag for SQL statement.
flagIsSql :: Flag -> Bool
flagIsSql (TheSql _) = True
flagIsSql _          = False

options :: [OptDescr Flag]
options =
     [ Option []        [optionNameObjectType] (ReqArg TheObjectType "OBJECT-TYPE")   "What ObjectType to operate on"
     , Option []        ["sql"]                (ReqArg TheSql        "SQL-STMT")      ("Which SQL statement to print: " ++ show sqlStatement_all)
     , Option ['V','?'] ["version"]            (NoArg Version)                        "Show version number"
     ]

-- | The name of the option for selecting an object type (or equivalently).
optionNameObjectType :: String
optionNameObjectType = "objecttype"
