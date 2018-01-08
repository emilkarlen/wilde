-- | Helper for constructing an ObjectType that consists of
--
-- * ID
--
-- * Date
--
-- * Title 
--
-- * Description
--
module Common.ObjectType.DateTitleDescription
       (
         OtNative
       , DtdObjectTypeSetup
       , ots       
       )
       where



-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Database.Sql

import           Wilde.ObjectModel.ObjectModelUtils

import           Wilde.ApplicationConstruction.StandardServices.Tools as SS
import           Wilde.ApplicationConstruction.ObjectModel.ObjectType
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS

import CashflowObjectModelUtils

import TitleUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type OtNative = ()

type DtdObjectTypeSetup dbTable = StdAutoPkObjectTypeSetup_ddl dbTable OtNative

ots :: (SQL_IDENTIFIER dbTable) 
    => (dbTable,dbTable,dbTable,dbTable) 
    -> SqlIdentifier
    -> String
    -> DtdObjectTypeSetup dbTable
ots (colId,colDate,colTitle,colDescription)
  tableName title =
  SS.objectTypeSetup otDtd 
  (withSingleClassStyle tableName title)
  `SS.withObjectListDisplaySetup`
  (OLS.ObjectListDisplaySetup
   dtd_listDisplayAts
   [Any at_date]
   numberOfObjectsFooterRow)
  where
    otDtd = ot_autoPk
             (databaseTable tableName)
             dtd_toNative
             (at_PrimaryKey_dbAutogen' colId)
             [
               Any at_date,
               Any at_title,
               Any at_description
             ]

    dtd_listDisplayAts = [Any at_date
                         ,Any at_title
                         ]

    at_date        = dtd_date colDate
    at_title       = dtd_title colTitle
    at_description = dtd_description colDescription

    dtd_toNative = ObjectToNativeFunction $ \o -> return ()

dtd_date :: (SQL_IDENTIFIER dbTable) 
         => dbTable -> PlainAttributeType_ddl dbTable Day
dtd_date col = at_Date_withConvenienteUiInput inputWidth_date col noDefault "Datum"

dtd_title :: (SQL_IDENTIFIER dbTable) 
          => dbTable -> PlainAttributeType_ddl dbTable String
dtd_title = at_Name "Namn"

dtd_description :: (SQL_IDENTIFIER dbTable) 
                => dbTable -> PlainAttributeType_optional_ddl dbTable String
dtd_description col = at_Text_html_optional my_dbIo_string (40,5) col noDefault "Beskrivning"
