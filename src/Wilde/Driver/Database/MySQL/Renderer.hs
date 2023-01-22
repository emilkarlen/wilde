module Wilde.Driver.Database.MySQL.Renderer
       (
         renderer,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC.ColTypes

import qualified Wilde.Driver.Database.RenderDdlUtils as RenderDdlUtils

import Wilde.Database.BackEndDdl hiding (colType)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


renderer :: DdlRenderer
renderer =
     DdlRenderer
     {
       render = RenderDdlUtils.renderDdl colTypeTranslator
     }

colTypeTranslator :: RenderDdlUtils.ColTypeTranslator
colTypeTranslator colDesc = fmap (++nullModifier) typ
  where
    typ = case colType colDesc of
      -- Fixed-width character strings
      SqlCharT -> char
      -- Variable-width character strings
      SqlVarCharT -> varchar
      -- Variable-width character strings, max length implementation dependant
      SqlLongVarCharT -> text
      -- Fixed-width Unicode strings
      SqlWCharT -> char
      -- Variable-width Unicode strings
      SqlWVarCharT -> varchar
      -- Variable-width Unicode strings, max length implementation dependant
      SqlWLongVarCharT -> text
      -- Signed exact values
      SqlDecimalT -> notImplemented
      -- Signed exact integer values
      SqlNumericT -> int
      -- 16-bit integer values
      SqlSmallIntT -> smallint
      -- 32-bit integer values
      SqlIntegerT -> int
      SqlRealT -> real
      -- Signed inexact floating-point values
      SqlFloatT -> real
      -- Signed inexact double-precision values
      SqlDoubleT -> real
      -- A single bit
      SqlBitT -> tinyint
      -- 8-bit integer values
      SqlTinyIntT -> tinyint
      -- 64-bit integer values
      SqlBigIntT -> int
      -- Fixed-length binary data
      SqlBinaryT -> notImplemented
      -- Variable-length binary data
      SqlVarBinaryT -> notImplemented
      -- Variable-length binary data, max length implementation dependant
      SqlLongVarBinaryT -> notImplemented
      -- A date
      SqlDateT -> date
      -- A time, no timezone
      SqlTimeT -> time
      -- A time, with timezone
      SqlTimeWithZoneT -> time
      -- Combined date and time, no timezone
      SqlTimestampT -> time
      -- Combined date and time, with timezone
      SqlTimestampWithZoneT -> time
      -- UTC date/time
      SqlUTCDateTimeT -> time
      -- UTC time
      SqlUTCTimeT -> time
      -- A time or date difference
      -- SqlIntervalT SqlInterval -> undefined
      -- Global unique identifier
      SqlGUIDT -> int
      SqlUnknownT s -> return s
    notImplemented = error $ "Not implemented: MySQL translation of column type " ++
                     (show (colType colDesc))
    nullModifier :: String
    nullModifier = maybe "" mkNullable $ colNullable colDesc
    mkNullable b = if b
                   then ""
                   else " NOT NULL"
    char        = withSize mkChar
    varchar     = withSize mkVarchar
    text        = return "TEXT"
    int         = return "INT"
    smallint    = return "SMALLINT"
    tinyint     = return "TINYINT"
    date        = return "DATE"
    time        = return "TIME"
    real        = return "REAL"
    --size        = maybe (Left sizeMissing) Right (colSize colDesc)
    sizeMissing = "MySQL: size is missing: " ++ show colDesc

    withSize :: (Int -> String) -> Either RenderDdlUtils.ErrorMessage String
    withSize mkType = maybe (error sizeMissing) (return . mkType) (colSize colDesc)

mkChar :: Int -> String
mkChar size = "CHAR(" ++ show size ++ ")"

mkVarchar :: Int -> String
mkVarchar size = "VARCHAR(" ++ show size ++ ")"
