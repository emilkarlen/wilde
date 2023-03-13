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


import           Wilde.Database.Sql

import           Wilde.ApplicationConstruction.ObjectModel.ObjectType

import qualified Common.ObjectType.AttributeType as CommonAT


------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


status_optional :: SQL_IDENTIFIER dbTable
                => dbTable
                -> PlainAttributeType_optional_ddl dbTable CommonAT.EnumNative
status_optional column = CommonAT.enum_optional values "Status" column
  where
    values = [(1,  "-") -- Have seen
             ,(2,  "/") -- Partly seen
             ,(10, "*") -- Want to see
             ]

media_status_optional :: SQL_IDENTIFIER dbTable
                      => dbTable
                      -> PlainAttributeType_optional_ddl dbTable CommonAT.EnumNative
media_status_optional column = CommonAT.enum_optional values "Media status" column
  where
    values = [
      (0, "orginal"),
      (1, "RW"),
      (2, "R/open"),
      (3, "R/closed")
      ]

media_optional :: SQL_IDENTIFIER dbTable
               => dbTable
               -> PlainAttributeType_optional_ddl dbTable CommonAT.EnumNative
media_optional column = CommonAT.enum_optional values "Media" column
  where
    values = [
      (1, "dvd"),
      (2, "video"),
      (3, "harddrive")
      ]

year_optional :: SQL_IDENTIFIER dbTable
              => dbTable
              -> PlainAttributeType_optional_ddl dbTable Word32
year_optional column = CommonAT.word32_optional "Year" column
