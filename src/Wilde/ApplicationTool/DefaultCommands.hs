{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------
-- | A list of default commands of the Application Tool
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.DefaultCommands
       (
         commands,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.ElementSet as ES (empty,ElementSet)

import Wilde.ApplicationTool.Commands.Check

import Wilde.ObjectModel.ObjectModelUtils

import Wilde.ApplicationTool.ApplicationModel
import Wilde.ApplicationTool.Command

import qualified Wilde.ApplicationTool.Commands.Database as DatabaseCommands


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


commands :: Commands ObjectModel
commands = [
  ("sql",
   ("Prints SQL",
    withParsedArgs DatabaseCommands.printSql)
  ),
  ("db-select-all-sqlvalue",
   ("Reads all records from the database and displays them as [SqlValue]",
    withParsedArgs DatabaseCommands.dbSelectAll_sqlRecord)
  ),
  ("db-select-all-tuplestring",
   ("Reads all records from the database and displays them as 'tuple strings'",
    withParsedArgs DatabaseCommands.dbSelectAll_tupleString)
  ),
  ("attribute-types",
   ("Reads all records from the database and displays the ID of each attribute type (???)",
    withParsedArgs DatabaseCommands.printAttributeTypes)
  ),
  ("info",
   ("Displays info about an ObjectType",
    withParsedArgs DatabaseCommands.printObjectTypeInfo)
  ),
  ("create-tables",
   ("Prints SQL CREATE TABLE statements for objecttypes.",
    withParsedArgs DatabaseCommands.printCreateTables)
  ),
  ("lo",(
      "List object types",
      \(CommandEnv { objectModel = om }) _ -> mapM_ (putStrLn . anyAnyApply otCrossRefKey) (objectTypes om)
      )
  )
  ,
  ("check",
   (
     "Checks the model of integrity: uniqueness of object-id:s, attribute-id:s, table-columns",
     withParsedArgs checkObjectModel
   )
  )
  ]

customEnvironment :: ES.ElementSet
customEnvironment = ES.empty
