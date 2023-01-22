{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

-------------------------------------------------------------------------------
-- | \"Config\" of database IO for types.
--
-- The structure 'DatabaseColumnType' holds information about a single type.
-- Such a structure is defined here for some common types one might want to
-- in use an application.
--
-- Import this module qualified.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.Database.DatabaseColumnTypes
       (
         -- * Convenience types

         DatabaseColumnType(..),

         -- * Construction of 'Database.OutputerWithConnection'

         mkOutputerWithConnection,

         -- * Info for some types

         -- ** Numbers

         -- *** Int

       int,
       int_optional,

       int32,
       int32_optional,

       int64,
       int64_optional,

         -- *** Word

       word32,
       word32_optional,

       word64,
       word64_optional,

         -- *** Floating point

       double,
       double_optional,

         -- ** Strings

         -- *** Variable length String with given max size


       string,
       string_forDbIo,
       dbIo_string_default,
       dbIo_string_byteString_utf8,
       colDesc_string,

       string_optional,
       string_optional_forDbIo,

       dbIo_string_optional_default,
       colDesc_string_optional,

         -- *** Variable length String with impl dependent max size

       longString,
       longString_optional,

       longString_forDefaultDbIo,
       longString_optional_forDefaultDbIo,

         -- ** Misc

         -- *** Bool

       bool,
       bool_optional,

         -- *** Date

       day,
       day_optional,

       -- * Utilities

       mkOptional,
       dbIo_mkOptional,

       numberOfSqlValuesError,
       invalidSqlValuesError,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Typeable
import Data.Convertible.Base

import Data.Word
import Data.Int
import Data.Time.Calendar (Day)
import qualified Data.ByteString.UTF8 as BsUtf8

import Database.HDBC.ColTypes
import Database.HDBC

import Wilde.Media.Database
import qualified Wilde.Media.Database.Monad as DbConn

import qualified Wilde.ObjectModel.Database as Database


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Basic information for a given type: IO and a 'SqlColDesc'.
data DatabaseColumnType a =
  (Typeable a, Show a) =>
  DatabaseColumnType
  {
    databaseIo        :: DatabaseIo a
  , columnDescription :: SqlColDesc
  }


-------------------------------------------------------------------------------
-- - Int -
-------------------------------------------------------------------------------


int :: DatabaseColumnType Int
int =
  DatabaseColumnType
  {
    databaseIo        = dbIo_convertible
  , columnDescription = sqlColDesc_integral SqlNumericT Nothing False
  }

int_optional :: DatabaseColumnType (Maybe Int)
int_optional = mkOptional int


-------------------------------------------------------------------------------
-- - Int32 -
-------------------------------------------------------------------------------


int32 :: DatabaseColumnType Int32
int32 =
  DatabaseColumnType
  {
    databaseIo        = dbIo_convertible
  , columnDescription = sqlColDesc_integral SqlIntegerT (Just 32) False
  }

int32_optional :: DatabaseColumnType (Maybe Int32)
int32_optional = mkOptional int32


-------------------------------------------------------------------------------
-- - Int64 -
-------------------------------------------------------------------------------


int64 :: DatabaseColumnType Int64
int64 =
  DatabaseColumnType
  {
    databaseIo        = dbIo_convertible
  , columnDescription = sqlColDesc_integral SqlBigIntT (Just 64) False
  }

int64_optional :: DatabaseColumnType (Maybe Int64)
int64_optional = mkOptional int64


-------------------------------------------------------------------------------
-- - Word32 -
-------------------------------------------------------------------------------


word32 :: DatabaseColumnType Word32
word32 =
  DatabaseColumnType
  {
    databaseIo        = dbIo_convertible
  , columnDescription = sqlColDesc_integral SqlIntegerT (Just 32) False
  }

word32_optional :: DatabaseColumnType (Maybe Word32)
word32_optional = mkOptional word32


-------------------------------------------------------------------------------
-- - Word64 -
-------------------------------------------------------------------------------


word64 :: DatabaseColumnType Word64
word64 =
  DatabaseColumnType
  {
    databaseIo        = dbIo_convertible
  , columnDescription = sqlColDesc_integral SqlBigIntT (Just 64) False
  }

word64_optional :: DatabaseColumnType (Maybe Word64)
word64_optional = mkOptional word64


-------------------------------------------------------------------------------
-- - Double -
-------------------------------------------------------------------------------


double :: DatabaseColumnType Double
double =
  DatabaseColumnType
  {
    databaseIo        = dbIo_convertible
  , columnDescription = SqlColDesc SqlDoubleT Nothing Nothing Nothing (Just False)
  }

double_optional :: DatabaseColumnType (Maybe Double)
double_optional = mkOptional double


-------------------------------------------------------------------------------
-- - String -
-------------------------------------------------------------------------------


string :: Int -- ^ Max string length
       -> DatabaseColumnType String
string = string_forDbIo dbIo_string_default

string_forDbIo :: DatabaseIo String
               -> Int -- ^ Max string length
               -> DatabaseColumnType String
string_forDbIo dbIo maxLen =
  DatabaseColumnType
  {
    databaseIo        = dbIo
  , columnDescription = colDesc_string maxLen
  }

dbIo_string_default :: DatabaseIo String
dbIo_string_default = dbIo_string_byteString_utf8

colDesc_string :: Int -- ^ Max string length
               -> SqlColDesc
colDesc_string maxLen = SqlColDesc SqlVarCharT (Just maxLen) Nothing Nothing (Just False)


-------------------------------------------------------------------------------
-- - Maybe String -
-------------------------------------------------------------------------------


string_optional :: Int -- ^ Max string length
                -> DatabaseColumnType (Maybe String)
string_optional maxLen = string_optional_forDbIo dbIo_string_optional_default maxLen

string_optional_forDbIo :: DatabaseIo (Maybe String)
                        -> Int -- ^ Max string length
                        -> DatabaseColumnType (Maybe String)
string_optional_forDbIo dbIo maxLen =
  DatabaseColumnType
  {
    databaseIo        = dbIo
  , columnDescription = colDesc_string_optional maxLen
  }

dbIo_string_optional_default :: DatabaseIo (Maybe String)
dbIo_string_optional_default = dbIo_mkOptional dbIo_string_default

colDesc_string_optional :: Int -- ^ Max string length
                        -> SqlColDesc
colDesc_string_optional = colDesc_mkOptional . colDesc_string

-- | `DatabaseIo` for a String, stored as
-- a `ByteString` and encoded using UTF-8
dbIo_string_byteString_utf8 :: DatabaseIo String
dbIo_string_byteString_utf8 =
  DatabaseIo
  {
    dbOutputer = dbO_string_byteString_utf8
  , dbInputer  = dbI_string_byteString_utf8
  }

dbI_string_byteString_utf8 :: DatabaseInputer String
dbI_string_byteString_utf8 [sqlValue] = case sqlValue of
  (SqlString x)     -> return x
  (SqlByteString x) -> return $ BsUtf8.toString x
  sqlValue          -> Left $
                       invalidSqlValuesError
                       "String"
                       ["SqlString","SqlByteString"]
                       sqlValue
dbI_string_byteString_utf8 sqlValues = Left $ numberOfSqlValuesError "String" sqlValues

dbO_string_byteString_utf8 :: DatabaseOutputer String
dbO_string_byteString_utf8 x = return [SqlByteString $ BsUtf8.fromString x]

-------------------------------------------------------------------------------
-- - LongString -
-------------------------------------------------------------------------------


-- | A long string.  Max length is implementation dependent.
longString :: DatabaseIo String
           -> DatabaseColumnType String
longString dbIo =
  DatabaseColumnType
  {
    databaseIo        = dbIo
  , columnDescription = SqlColDesc SqlLongVarCharT Nothing Nothing Nothing (Just False)
  }

longString_optional :: DatabaseIo String
                    -> DatabaseColumnType (Maybe String)
longString_optional = mkOptional . longString

-- | A long string.  Max length is implementation dependent.
longString_forDefaultDbIo :: DatabaseColumnType String
longString_forDefaultDbIo =
  DatabaseColumnType
  {
    databaseIo        = dbIo_string_default
  , columnDescription = SqlColDesc SqlLongVarCharT Nothing Nothing Nothing (Just False)
  }

longString_optional_forDefaultDbIo :: DatabaseColumnType (Maybe String)
longString_optional_forDefaultDbIo = mkOptional longString_forDefaultDbIo


-------------------------------------------------------------------------------
-- - Bool -
-------------------------------------------------------------------------------


bool :: DatabaseColumnType Bool
bool =
  DatabaseColumnType
  {
    databaseIo        = dbIo_Bool
  , columnDescription = sqlColDesc_integral SqlBitT (Just 1) False
  }

bool_optional :: DatabaseColumnType (Maybe Bool)
bool_optional = mkOptional bool

dbIo_Bool :: DatabaseIo Bool
dbIo_Bool =
  DatabaseIo
  {
    dbOutputer = outputer
  , dbInputer  = inputer
  }
  where
    outputer :: DatabaseOutputer Bool
    outputer x = return [SqlWord32 (if x then 1 else 0)]

    inputer  :: DatabaseInputer Bool
    inputer [x] = safeFromSql x
    inputer xs  = Left $ numberOfSqlValuesError "Bool" xs


-------------------------------------------------------------------------------
-- - Day -
-------------------------------------------------------------------------------


day :: DatabaseColumnType Day
day =
  DatabaseColumnType
  {
    databaseIo        = dbIo_convertible
  , columnDescription = SqlColDesc SqlDateT Nothing Nothing Nothing (Just False)
  }

day_optional :: DatabaseColumnType (Maybe Day)
day_optional = mkOptional day


-------------------------------------------------------------------------------
-- - Convertible -
-------------------------------------------------------------------------------


dbIo_convertible :: (Typeable a,Convertible SqlValue a,Convertible a SqlValue)
                 => DatabaseIo a
dbIo_convertible =
   DatabaseIo
   {
     dbOutputer = outputer
   , dbInputer  = inputer
   }
   where
     outputer :: (Convertible SqlValue a,Convertible a SqlValue)
              => DatabaseOutputer a
     outputer x =
       do
         v <- safeConvert x
         return [v]

     inputer :: (Typeable a,Convertible SqlValue a)
             => DatabaseInputer a
     inputer [sqlValue] = safeFromSql sqlValue
     inputer xs = inputTooManyValues undefined xs

dbIo_convertible_optional :: (Typeable a,Convertible SqlValue a,Convertible a SqlValue)
                          => DatabaseIo (Maybe a)
dbIo_convertible_optional =
   DatabaseIo
   {
     dbOutputer = outputer
   , dbInputer  = inputer
   }
  where
    outputer :: (Convertible SqlValue a,Convertible a SqlValue)
             => DatabaseOutputer (Maybe a)
    outputer x = do
      v <- maybe (return SqlNull) safeConvert x
      return [v]

    inputer :: (Typeable a,Convertible SqlValue a)
            => DatabaseInputer (Maybe a)
    inputer [SqlNull]  = return Nothing
    inputer [sqlValue] = fmap Just $ safeFromSql sqlValue
    inputer xs         = inputTooManyValues undefined xs


-------------------------------------------------------------------------------
-- - Utilities related to DatabaseColumnType -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Transforms a 'DatabaseColumnType' of a mandatory type to one for
-- the corresponding optional type.
--
-- The 'Typeable' constraint exist only for ability to include a type
-- description into error messages.
-------------------------------------------------------------------------------
mkOptional :: Typeable a
           => DatabaseColumnType a
           -> DatabaseColumnType (Maybe a)
mkOptional (DatabaseColumnType dbIo colDesc)  =
  DatabaseColumnType
  {
    databaseIo        = dbIo_mkOptional dbIo
  , columnDescription = colDesc_mkOptional colDesc
  }

dbIo_mkOptional :: Typeable a
                => DatabaseIo a
                -> DatabaseIo (Maybe a)
dbIo_mkOptional (DatabaseIo outputer_m inputer_m) =
   DatabaseIo
   {
     dbOutputer = outputer
   , dbInputer  = inputer
   }
  where
    outputer x = do
      vs <- (maybe (return [SqlNull]) outputer_m) x
      return vs

    inputer [SqlNull]  = return Nothing
    inputer [sqlValue] = fmap Just $ inputer_m [sqlValue]
    inputer xs         = inputTooManyValues undefined xs

-- | Transforms a 'SqlColDesc' for a mandatory typ to one for the
-- corresponding optional type.
colDesc_mkOptional :: SqlColDesc -- ^ For the mandatory type - Null is not allowed
                   -> SqlColDesc
colDesc_mkOptional x = x { colNullable = Just True }


-------------------------------------------------------------------------------
-- - Utilities related to integral types -
-------------------------------------------------------------------------------


-- A 'SqlColDesc' for an \"integral\" type - some kind of integer.
sqlColDesc_integral :: SqlTypeId    -- ^ Type for integral type.
                    -> Maybe Int -- ^ Number of bits needed.
                    -> Bool      -- ^ If NULL is allowed.
                    -> SqlColDesc
sqlColDesc_integral sqlTypeId mbNumBits bNullAllowed =
  SqlColDesc sqlTypeId mbNumBits Nothing Nothing (Just bNullAllowed)


-------------------------------------------------------------------------------
-- - mkOutputerWithConnection -
-------------------------------------------------------------------------------


-- | Constructs a 'Database.OutputerWithConnection'.
mkOutputerWithConnection :: Typeable a
                         => DatabaseOutputer a
                         -> Database.OutputerWithConnection a
mkOutputerWithConnection outputer value =
  case outputer value of
    Left err -> DbConn.throwErr $ AttributeTranslationError errorInfo err
      where
        errorInfo = "Type: " ++ show (typeOf value)
    Right sqlValues -> return sqlValues


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


-- Purpose of this function is just to get a value of type a so that
-- we can include it's TypeRep in the error message.
inputTooManyValues :: (Typeable a)
                   => a
                   -> DatabaseInputer a
inputTooManyValues valueForTypeOf xs = Left $ numberOfSqlValuesError typeDesc xs
  where
    typeDesc = show $ typeOf valueForTypeOf

-- | Constructs a ConvertError for the error "invalid number of SQL values".
numberOfSqlValuesError :: String
                       -> [SqlValue]
                       -> ConvertError
numberOfSqlValuesError destTypeDescr actualInput =
  ConvertError
  {
    convSourceValue  = show actualInput
  , convSourceType   = "[SqlValue]"
  , convDestType     = destTypeDescr
  , convErrorMessage = "Invalid number of element in list of SqlValue"
  }

-- | Constructs a ConvertError for the error "invalid number of SQL values".
invalidSqlValuesError :: String
                      -> [String]
                      -> SqlValue
                      -> ConvertError
invalidSqlValuesError destTypeDescr expectedTypes actualInput =
  ConvertError
  {
    convSourceValue  = show actualInput
  , convSourceType   = "[SqlValue]"
  , convDestType     = destTypeDescr
  , convErrorMessage = "Invalid SqlValue. Expected any of " ++ show expectedTypes
  }
