module Video.ObjectType.AttributeType
       (
         status_optional,
         media_status_optional,
         media_optional,
         year_optional,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Database.Sql

import           Wilde.ObjectModel.ObjectModelUtils

import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType
import           Wilde.ApplicationConstruction.ObjectModel.ObjectType

import qualified Common.ObjectType.AttributeType as CommonAT
-- import qualified Pills.ObjectType.AttributeType as PillsAT


------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


status_optional :: SQL_IDENTIFIER dbTable
                => dbTable
                -> PlainAttributeType_optional_ddl dbTable CommonAT.EnumNative
status_optional column = CommonAT.enum_optional values "Status" column
  where
    values = [(1,  "-") -- Har sett
             ,(2,  "/") -- Sett delvis
             ,(10, "*") -- Viss se
             ]

media_status_optional :: SQL_IDENTIFIER dbTable
                      => dbTable
                      -> PlainAttributeType_optional_ddl dbTable CommonAT.EnumNative
media_status_optional column = CommonAT.enum_optional values "Media status" column
  where
    values = [
      (0, "orginal"),
      (1, "RW"),
      (2, "R/öppen"),
      (3, "R/sluten")
      ]

media_optional :: SQL_IDENTIFIER dbTable
               => dbTable
               -> PlainAttributeType_optional_ddl dbTable CommonAT.EnumNative
media_optional column = CommonAT.enum_optional values "Media" column
  where
    values = [
      (1, "dvd"),
      (2, "video"),
      (3, "hårddisk")
      ]

year_optional :: SQL_IDENTIFIER dbTable
              => dbTable
              -> PlainAttributeType_optional_ddl dbTable Word32
year_optional column = CommonAT.word32_optional "År" column
