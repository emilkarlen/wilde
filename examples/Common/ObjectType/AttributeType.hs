module Common.ObjectType.AttributeType
       (
         primaryKey_dbAutogen,
         my_at_String,
         my_at_String_optional,

         bool,
         bool_optional,

         word32,
         word32_optional,

         date,
         name,
         name_optional,

         summary_optional,

         description_optional,

         text_optional,

         double,
         double_optional,

         amount,
         amount_optional,

         percentage_optional,
         
         EnumNative,
         enum,
         enum_optional,

         href,
         href_optional,
         
         my_dbIo_string,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import Data.Word

import qualified Data.ByteString.Char8 as Char8

import Text.Printf (printf)

import Wilde.Database.Sql
import Wilde.Media.Database

import qualified Wilde.ObjectModel.UserInteraction as UserInteraction
import Wilde.ApplicationConstruction.ObjectModel.AttributeType hiding (description_optional)
import Wilde.ApplicationConstruction.Presentation.Presentation (asUnquotedString, asUnquotedString_optional)
import Wilde.Media.WildeMedia (PresentationOutputer)
import Wilde.ApplicationConstruction.Database.DatabaseColumnTypes (dbIo_mkOptional, numberOfSqlValuesError, invalidSqlValuesError)



-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


inputWidth_primaryKey :: Int
inputWidth_primaryKey = 10

inputWidth_word32 :: Int
inputWidth_word32 = 5

maxSize_name :: Int
maxSize_name = 50

inputWidth_name :: Int
inputWidth_name = 40

maxSize_href :: Int
maxSize_href = 50

inputWidth_href :: Int
inputWidth_href = 40

inputWidth_date :: Int
inputWidth_date = 12

maxSize_amount :: Int
maxSize_amount = 100

inputWidth_amount :: Int
inputWidth_amount = 40

maxSize_summary :: Int
maxSize_summary = 100

inputWidth_summary :: Int
inputWidth_summary = 50

inputSize_text :: (Int,Int)
inputSize_text = (50,6)

primaryKey_dbAutogen :: SQL_IDENTIFIER dbTable
                     => dbTable
                     -> StdAutoPkPkAttributeType_ddl dbTable
primaryKey_dbAutogen = at_PrimaryKey_dbAutogen 10

date :: SQL_IDENTIFIER dbTable
     => Title
     -> dbTable
     -> PlainAttributeType_ddl dbTable Day
date title column = at_Date_withConvenienteUiInput inputWidth_date column noDefault title

bool :: SQL_IDENTIFIER dbTable
     -- => Maybe (UserInteraction.AttributeTypeCreateOption Bool)
     => Title
     -> dbTable
     -> PlainAttributeType_ddl dbTable Bool
bool = at_Bool_checkBox Nothing

bool_optional :: SQL_IDENTIFIER dbTable
              -- => Maybe (UserInteraction.AttributeTypeCreateOption (Maybe Bool))
              => Title
              -> dbTable
              -> PlainAttributeType_optional_ddl dbTable Bool
bool_optional = at_Bool_optional_dropDown Nothing

word32 :: SQL_IDENTIFIER dbTable
       => Title
       -> dbTable
       -> PlainAttributeType_ddl dbTable Word32
word32 title column = at_Word32 inputWidth_word32 column Nothing title

word32_optional :: SQL_IDENTIFIER dbTable
                => Title
                -> dbTable
                -> PlainAttributeType_optional_ddl dbTable Word32
word32_optional title column = at_Word32_optional inputWidth_word32 column Nothing title

href :: SQL_IDENTIFIER dbTable
      => Title
      -> dbTable
      -> PlainAttributeType_ddl dbTable String
href title column = at_Href my_dbIo_string maxSize_href inputWidth_href column Nothing title 

href_optional :: SQL_IDENTIFIER dbTable
                 => Title
                 -> dbTable
                 -> PlainAttributeType_optional_ddl dbTable String
href_optional title column = at_Href_optional my_dbIo_string_optional maxSize_href inputWidth_href column Nothing title 

name :: SQL_IDENTIFIER dbTable
     => Title
     -> dbTable
     -> PlainAttributeType_ddl dbTable String
name = my_at_String maxSize_name inputWidth_name

name_optional :: SQL_IDENTIFIER dbTable
              => Title
              -> dbTable
              -> PlainAttributeType_optional_ddl dbTable String
name_optional = my_at_String_optional maxSize_name inputWidth_name

summary_optional :: SQL_IDENTIFIER dbTable
                 => Title
                 -> dbTable
                 -> PlainAttributeType_optional_ddl dbTable String
summary_optional = my_at_String_optional maxSize_summary inputWidth_summary

description_optional :: SQL_IDENTIFIER dbTable
                     => Title
                     -> dbTable
                     -> PlainAttributeType_optional_ddl dbTable String
description_optional = text_optional

text_optional :: SQL_IDENTIFIER dbTable
              => Title
              -> dbTable
              -> PlainAttributeType_optional_ddl dbTable String
text_optional title column = at_Text_optional
                             my_dbIo_string
                             inputSize_text
                             column
                             Nothing
                             title

double :: SQL_IDENTIFIER dbTable
       => Title
       -> dbTable
       -> PlainAttributeType_ddl dbTable Double
double = amount

double_optional :: SQL_IDENTIFIER dbTable
                => Title
                -> dbTable
                -> PlainAttributeType_optional_ddl dbTable Double
double_optional = amount_optional

amount :: SQL_IDENTIFIER dbTable
       => Title
       -> dbTable
       -> PlainAttributeType_ddl dbTable Double
amount title column = at_Double_withExprUiInput
                      inputWidth_amount
                      column
                      Nothing
                      title

amount_optional :: SQL_IDENTIFIER dbTable
                => Title
                -> dbTable
                -> PlainAttributeType_optional_ddl dbTable Double
amount_optional title column = at_Double_optional_withExprUiInput
                               inputWidth_amount
                               column
                               Nothing
                               title

percentage :: SQL_IDENTIFIER dbTable
           => Title
           -> dbTable
           -> PlainAttributeType_ddl dbTable Double
percentage title column = amount_at { atPresentationO = percentagePresenter }
  where
    amount_at = amount title column
    percentagePresenter :: PresentationOutputer Double
    percentagePresenter = asUnquotedString percentage_formatter

percentage_optional :: SQL_IDENTIFIER dbTable
                    => Title
                    -> dbTable
                    -> PlainAttributeType_optional_ddl dbTable Double
percentage_optional title column = amount_at { atPresentationO = percentagePresenter }
  
  where
    amount_at = amount_optional title column
    percentagePresenter :: PresentationOutputer (Maybe Double)
    percentagePresenter = asUnquotedString_optional percentage_formatter

percentage_formatter :: Double -> String
percentage_formatter = (++"%") . printf "%3.2f" . (*100.0)

-- amount :: SQL_IDENTIFIER dbTable
--        => Title
--        -> dbTable
--        -> PlainAttributeType_ddl dbTable String
-- amount = my_at_String maxSize_amount inputWidth_amount

-- amount_optional :: SQL_IDENTIFIER dbTable
--                 => Title
--                 -> dbTable
--                 -> PlainAttributeType_optional_ddl dbTable String
-- amount_optional = my_at_String_optional maxSize_amount inputWidth_amount

type EnumNative = Word32

enum :: SQL_IDENTIFIER dbTable
     => [(Word32,String)]
     -> Title
     -> dbTable
     -> PlainAttributeType_ddl dbTable EnumNative
enum values title column =
  at_EnumAsDropDown_Word32 values' column
  noDefault
  (withNeutralWildeStyle title)
  where
    values' = map (\(n,str) -> (n,unquotedDropDownValue str)) values

enum_optional :: SQL_IDENTIFIER dbTable
              => [(Word32,String)]
              -> Title
              -> dbTable
              -> PlainAttributeType_optional_ddl dbTable EnumNative
enum_optional values title column =
  at_EnumAsDropDown_Word32_optional values' column
  noDefault
  (withNeutralWildeStyle title)
  where
    values' = map (\(n,str) -> (n,unquotedDropDownValue str)) values

-- mkIdNameConfig :: StyledTitle
--                -> SqlIdentifier -- ^ Database Table Name
--                -> dbTable       -- ^ PK attribute
--                -> dbTable       -- ^ Name attribute
--                -> ToolsMySql.IdNameOtConfiguration dbTable 
-- mkIdNameConfig objectTypeTitle dbTableName colPk colName =
--   ToolsMySql.IdNameOtConfiguration
--   {
--     ToolsMySql.objectTypeTitle = objectTypeTitle
--   , ToolsMySql.nameTitle       = "Namn"
--   , ToolsMySql.maxSize         = maxSize_name
--   , ToolsMySql.inputWidth      = inputWidth_name
--   , ToolsMySql.dbTableName     = dbTableName
--   , ToolsMySql.colPk           = colPk
--   , ToolsMySql.colName         = colName
--   , ToolsMySql.dbIo_string     = my_dbIo_string
--   }


-- mkIdNameConfig_std :: StyledTitle
--                    -> SqlIdentifier -- ^ Database Table Name
--                    -> ToolsMySql.IdNameOtConfiguration IdNameObjectType.IdNameTable
-- mkIdNameConfig_std objectTypeTitle dbTableName =
--   mkIdNameConfig
--   objectTypeTitle dbTableName
--   IdNameObjectType.IdColumn
--   IdNameObjectType.NameColumn

-- ots_and_rps_stdPkName :: SQL_IDENTIFIER dbTable
--                       => StyledTitle   -- ^ Title of Object Type
--                       -> SqlIdentifier -- ^ Database Table Name
--                       -> dbTable       -- ^ PK attribute
--                       -> dbTable       -- ^ Name attribute
--                       -> (IdNameObjectType.IdNamePresStrSpec_ddl     dbTable,
--                           IdNameObjectType.IdNameObjectTypeSetup_ddl dbTable)
-- ots_and_rps_stdPkName titleOt tableName colPk colName =
--   ToolsMySql.ots_and_rps_IdName_dbAutogen_MySql
--   (mkIdNameConfig titleOt tableName colPk colName)


-- negPosSumCells :: AnyCellConstructor Double
-- negPosSumCells = AnyCellConstructor negPosSumCellsRaw

-- negPosSumCellsRaw :: CellConstructor NegPosSumAcc Double
-- negPosSumCellsRaw = 
--   CellConstructor
--   {
--     fccInitial     = (fromInteger 0,fromInteger 0)
--   , fccAccumulator = \(x,acc@(neg',pos')) -> if x < 0
--                                              then (neg' + x,pos')
--                                              else (
--                                                if x > 0
--                                                then (neg',pos' + x)
--                                                else acc
--                                                )
--   , fccMkCell = \numObjects (neg,pos) -> if numObjects <= 1
--                                          then []
--                                          else concat [cellIfNonZero neg,
--                                                       cellIfNonZero pos,
--                                                       sumIfBothNonZero neg pos
--                                                      ]
--   }
--   where
--     cellIfNonZero :: Double -> [WildeStyledCell]
--     cellIfNonZero x = if x /= 0
--                       then mkCell x
--                       else []

--     sumIfBothNonZero x y = if x /= 0 && y /= 0
--                            then mkCell (x + y)
--                            else []

--     mkCell :: Double -> [WildeStyledCell]
--     mkCell x = [cellStyled (valueStyle svalue) svalue]
--       where
--         svalue = presentMoney x

-- type NegPosSumAcc = (Double,Double)


my_at_String :: SQL_IDENTIFIER table
             => Int -- ^ Max string length
             -> Int -- ^ Input width
             -> Title
             -> table
             -> PlainAttributeType_ddl table String
my_at_String maxSize inputWidth title column =
  at_String
  my_dbIo_string
  maxSize
  inputWidth
  column
  Nothing
  title

my_at_String_optional :: SQL_IDENTIFIER table
                      => Int -- ^ Max string length
                      -> Int -- ^ Input width
                      -> Title
                      -> table
                      -> PlainAttributeType_optional_ddl table String
my_at_String_optional maxSize inputWidth title column =
  at_String_optional
  my_dbIo_string_optional
  maxSize
  inputWidth
  column
  Nothing
  title

my_dbIo_string_optional :: DatabaseIo (Maybe String)
my_dbIo_string_optional = dbIo_mkOptional my_dbIo_string


my_dbIo_string :: DatabaseIo String
my_dbIo_string =
  DatabaseIo
  {
    dbOutputer = dbO_string
  , dbInputer  = dbI_string
  }

dbI_string :: DatabaseInputer String
dbI_string [sqlValue] = case sqlValue of
  (SqlString x)     -> return x
  (SqlByteString x) -> return $ Char8.unpack x
  sqlValue          -> Left $
                       invalidSqlValuesError
                       "String"
                       ["SqlString","SqlByteString"]
                       sqlValue
dbI_string sqlValues = Left $ numberOfSqlValuesError "String" sqlValues

dbO_string :: DatabaseOutputer String
dbO_string x = return [SqlByteString $ Char8.pack x]
