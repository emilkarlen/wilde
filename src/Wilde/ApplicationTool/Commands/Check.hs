-------------------------------------------------------------------------------
-- | A command of the Application Tool that checks an Object Model for
-- integrity and consistency.
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.Commands.Check
       (
         checkObjectModel,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Char

import qualified Data.Map as Map

import Data.List

import Control.Monad

import qualified Wilde.Media.Database as DbM
import Wilde.Database.Sql

import Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Database.Utils as DatabaseUtils

import qualified Wilde.ApplicationConstruction.StandardServices as StandardServices

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation as ObjectTypeWithAtDdlInformation

import Wilde.ApplicationTool.ApplicationModel
import Wilde.ApplicationTool.Command
import Wilde.ApplicationTool.Commands.Utils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | The command
checkObjectModel :: OBJECT_TYPE_SETUPS_WITH_DDL_INFO om
                 => CommandWithParsedArgs om
checkObjectModel (CommandEnv { objectModel = om }) _ =
  do
    let checks = checkObjectTypeSet objectTypes :
                 map checkObjectType objectTypes ++
                 map checkObjectTypeSetup objectTypeSetups
    passed <- checkList checks
    when (not passed) $ ioError $ userError "The Object Model has inconsistencies."
  where
    objectTypes      = objectTypesWithDdlInfo om
    objectTypeSetups = objectTypesWithStandardServiesWithDdlInfo om


-------------------------------------------------------------------------------
-- - types -
-------------------------------------------------------------------------------


-- ^ (Title of set of elements, Title/name/descr of single element)
type ElementsInfo = (String,String)

-- | A method that checks a list of elements, and returns wether they passed it or not.
type Checker a = ElementsInfo -> [a] -> IO Bool


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


-- | Applies a list of checks on a single type of elements.
applyCheckers :: ElementsInfo -> [a] -> [Checker a] -> IO Bool
applyCheckers elementsInfo elements checkers =
  do
    passed <- mapM (\check -> check elementsInfo elements) checkers
    return $ and passed

-- | Executes a list of check methods.
checkList :: [IO Bool] -> IO Bool
checkList = fmap and . sequence


ofObjectType :: String -> ObjectType otConf atConf dbT otN idAE idAC -> String
ofObjectType s ot = s ++ "s of Object Type " ++ quote (otCrossRefKey ot)


getAtKey :: Any (AttributeType atConf dbTable) -> CrossRefIdentifier
getAtKey (Any at) = atCrossRefKey at


-------------------------------------------------------------------------------
-- - checks -
-------------------------------------------------------------------------------


checkObjectTypeSet :: [ObjectTypeWithAtDdlInformation.AnyO ObjectType] -> IO Bool
checkObjectTypeSet ots =
  do
    warnAboutDuplicateTableNames ots
    checkObjectTypeKeys          ots

warnAboutDuplicateTableNames :: [ObjectTypeWithAtDdlInformation.AnyO ObjectType] -> IO ()
warnAboutDuplicateTableNames ots =
  do
    checkUniqueness
      ("Warning: Object Type Database Table Names",
       "Database Table Name")
      (map getTableName ots)
    return ()
  where
    getTableName :: ObjectTypeWithAtDdlInformation.AnyO ObjectType -> String
    getTableName (ObjectTypeWithAtDdlInformation.AnyO ot) = DbM.tableName $ Database.otDatabaseTable ot

checkObjectTypeKeys :: [ObjectTypeWithAtDdlInformation.AnyO ObjectType] -> IO Bool
checkObjectTypeKeys ots =
  let
    elements = map getCrossRefKey ots
    checkers = [checkUniqueness
               ,checkStringSyntax isValidCrossRefChar]
  in
   applyCheckers
   ("Object Type Keys",
    "Object Type Key")
   elements
   checkers
  where
    getCrossRefKey :: ObjectTypeWithAtDdlInformation.AnyO ObjectType -> CrossRefIdentifier
    getCrossRefKey (ObjectTypeWithAtDdlInformation.AnyO ot) = otCrossRefKey ot

checkObjectTypeSetup :: ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup -> IO Bool
checkObjectTypeSetup anyOts@(ObjectTypeWithAtDdlInformation.AnyO ots) =
  checkList [checkPresentationSingleAtList anyOts
            ]

checkObjectType :: ObjectTypeWithAtDdlInformation.AnyO ObjectType -> IO Bool
checkObjectType ota =
  checkList [checkAttributeTypeKeys ota
            ,checkColumnNames ota
            ]

checkAttributeTypeKeys :: ObjectTypeWithAtDdlInformation.AnyO ObjectType -> IO Bool
checkAttributeTypeKeys (ObjectTypeWithAtDdlInformation.AnyO ot) =
  let
    elementName = "Attribute Key"
    elements    = map getAtKey  $ otAttributeTypes ot
    checkers    = [checkUniqueness
                  ,checkStringSyntax isValidCrossRefChar
                  ]
  in
   applyCheckers
   (elementName `ofObjectType` ot,
    elementName)
   elements
   checkers

-- | Checks that if a oopsPresentationSingleAts is given as non-Nothing,
-- then it is a permutation of the 'AttributeType's of the
-- 'ObjectType'.
checkPresentationSingleAtList :: ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup -> IO Bool
checkPresentationSingleAtList (ObjectTypeWithAtDdlInformation.AnyO (StandardServices.ObjectTypeSetup
                                     {
                                       StandardServices.objectType          = ot
                                     , StandardServices.alternativeAtsOrder = alternativeAtsOrder
                                       })) =
  case alternativeAtsOrder of
    [] -> return True
    atsPresSingle ->
      let
        keysPresSingle = sort $ map getAtKey atsPresSingle
        keysAll        = sort $ map getAtKey $ otAttributeTypes ot
      in
       if keysPresSingle /= keysAll
       then do
         mapM_ putStrLn [otCrossRefKey ot ++ ": Presentation-single-attributes is not a permutation of all ats:",
                         "All ats     : " ++ show keysAll,
                         "Pres-single : " ++ show keysPresSingle
                        ]
         return False
       else return True

checkColumnNames :: ObjectTypeWithAtDdlInformation.AnyO ObjectType -> IO Bool
checkColumnNames (ObjectTypeWithAtDdlInformation.AnyO ot@(ObjectType {})) =
  let
    elementName = "Database Column Name"
    elements = map sqlIdentifier $ concatMap getDbFields  $ otAttributeTypes ot
    checkers = [checkUniqueness
               ,checkStringSyntax isValidSqlIdentifier]
  in
   applyCheckers
   (elementName `ofObjectType` ot,
    elementName)
   elements
   checkers
  where
    getDbFields :: Any (AttributeType DdlAtAnnotation.Configuration dbTable) -> [dbTable]
    getDbFields (Any at) =
      let dbCols = DatabaseUtils.atColumnList at
      in  map DbM.columnName dbCols

checkUniqueness :: Checker String
checkUniqueness (title,elementName) elements =
  do
    let nonUniques = getNonUniqueElements elements
    reportNonUnique nonUniques
    return $ null nonUniques
  where
    reportNonUnique :: [(String,Int)] -- ^ Elements and num occurencies.
                       -> IO ()
    reportNonUnique [] = return ()
    reportNonUnique nonUniques =
      do
        putStrLn $ title ++ ": " ++ show (length nonUniques) ++ " duplicates detected"
        let elementRows = map (\(elem,num) -> (elem,show num)) nonUniques
        print2ColTable (elementName,"Num Occurrences") elementRows

-- | Gives the elements which are non unique, and the
-- number of times (>1) they occurr.
getNonUniqueElements :: [String] -- ^ Elements to check
                        -> [(String,Int)]
getNonUniqueElements xs =
  let
    numOccursMap :: Map.Map String Int
    numOccursMap = foldl (\map s -> Map.insertWith (+) s 1 map) Map.empty xs
  in
   filter ((>1) . snd) $ Map.assocs numOccursMap

checkStringSyntax :: (Char -> Bool) -- ^ Tells if a character is valid
                  -> Checker String
checkStringSyntax isValidChar (title,elementName) elements =
   do
     let invalidStrings = filter (not . isValidString) elements
     reportInvalidSyntax invalidStrings
     return $ null invalidStrings
  where
    isValidString :: String -> Bool
    isValidString = and . map isValidChar

    reportInvalidSyntax :: [String] -> IO ()
    reportInvalidSyntax []             = return ()
    reportInvalidSyntax invalidStrings =
      do
        putStrLn $ title ++ ": " ++ show (length invalidStrings) ++ " with invalid syntax"
        let elementRows = map (\s -> (s,quote $ filter (not . isValidChar) s)) invalidStrings
        print2ColTable (elementName,"Invalid Characters") elementRows

isValidSqlIdentifier :: Char -> Bool
isValidSqlIdentifier ch = isAlphaNum ch || ch == '_'

isValidCrossRefChar :: Char -> Bool
isValidCrossRefChar ch = isAlphaNum ch || ch `elem` "_-"

quote :: String -> String
quote s = '"' : s ++ "\""

print2ColTable :: (String,String)      -- ^ Column titles
                  -> [(String,String)] -- ^ Data
                  -> IO ()
print2ColTable (t1,t2) rows =
  mapM_ (putStrLn . ("     "++)) (stdTable $ [t1,t2] : map (\(x,y) -> [x,y]) rows)
