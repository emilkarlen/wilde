-- | Utilities related to database SQL generation.
module Wilde.ObjectModel.Database.InputExistingSansPresentationInfo
       (
         COLUMN_NAMES(..),
         INPUT_FOR_EXISTING(..),
         IO_FOR_EXISTING(..),

         inputObject,
         inputObjectUtil,

         inputAttributeValue,
         inputAttribute,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.Error.Class

import Wilde.ObjectModel.ObjectModelUtils

import Wilde.Database.Sql
import Database.HDBC

import Wilde.Utils.ListUtils

import Wilde.Utils.Utils

import Wilde.Media.Database

import Wilde.ObjectModel.Database.Utils

import Wilde.ObjectModel.Database


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Reads an 'Object' of a given 'ObjectType' from a database record.
-------------------------------------------------------------------------------
inputObject :: (COLUMN_NAMES atConf
               ,INPUT_FOR_EXISTING atConf)
            => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
            -> DatabaseInput
            -> TranslationMonad (Object otConf atConf dbTable otNative idAtExisting idAtCreate)
inputObject ot = inputObjectUtil (ot,inputPlainObjectSetup ot)


inputObjectUtil :: INPUT_FOR_EXISTING atConf
                => (ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                   ,ObjectTranslationSetup atConf dbTable otNative idAtExisting idAtCreate
                   )
                -> [SqlValue]
                -> TranslationMonad (Object otConf atConf dbTable otNative idAtExisting idAtCreate)
inputObjectUtil (ot@(ObjectType {}),
                          ObjectTranslationSetup
                          {
                            setupIdAt            = atId
                          , setupIdAtColumns     = dbColsIdAt
                          , setupNonIdAts        = atNonIds
                          , setupNonIdAtsColumns = dbColGroupsPerNonIdAt
                          }
                         )
  dbRecord =
   case splitAs2 dbColGroupsPerAt dbRecord of
     Nothing             -> throwError $ recErr "Too few columns"
     Just (_,y:ys)       -> throwError $ recErr "Superfluous columns"
     Just ([],_)         -> throwError $ recErr "No groups, not even for mandatory ID attr"
     Just (sqlValuesForIdAt:sqlValuesForNonIdAts,_) ->
       do
         attrId     <- inputAttribute  atId sqlValuesForIdAt
         attrNonIds <- inputAttributes atNonIds sqlValuesForNonIdAts
         return $ conObject ot attrId (reverse attrNonIds)
  where
    recErr :: String -> TranslationError
    recErr head = let sExpected = show ((map . map) (fmap sqlIdentifier) dbColGroupsPerAt)
                      mismatch = Mismatch { expected = sExpected,
                                            actual   = show dbRecord
                                          }
                  in  RecordTranslationError head mismatch
    dbColGroupsPerAt = dbColsIdAt : dbColGroupsPerNonIdAt

-------------------------------------------------------------------------------
-- | Reads a list of 'Attributes' from 'SqlValue's.
--
-- See notes for 'inputAttribute'.
-------------------------------------------------------------------------------
inputAttributes :: INPUT_FOR_EXISTING atConf
                => [Any (AttributeType atConf dbTable)]
                -> [[SqlValue]]
                -> TranslationMonad [Any (Attribute atConf dbTable)]
inputAttributes ats sqlValuess = read ([],ats,sqlValuess)
  where
    read :: INPUT_FOR_EXISTING atConf
         => ([Any (Attribute atConf dbTable)],
             [Any (AttributeType atConf dbTable)],
             [[SqlValue]]) -- ^ (acc of attrs read so far, attr types to read, The SQL-values for each attribute)
         -> TranslationMonad [Any (Attribute atConf dbTable)]
    read (attrs,[],_) = return attrs
    read (attrs,(Any at):ats,colVals:colValss) =
      do
        attr <- inputAttribute at colVals
        read ((Any attr):attrs,ats,colValss)

-------------------------------------------------------------------------------
-- | Reads a single 'Attribute' from a list of 'SqlValue's.
--
-- The list of 'SqlValue's must be exactly what is used by the 'AttributeType':
--
-- * the length of the list == the number of values used by the 'AttributeType'
--
-- * the type of value at position i in the list == the expected type at
--   position i.
-------------------------------------------------------------------------------
inputAttribute :: INPUT_FOR_EXISTING atConf
               => AttributeType atConf dbTable typeForExisting typeForCreate
               -> [SqlValue]
               -> TranslationMonad (Attribute atConf dbTable typeForExisting typeForCreate)
inputAttribute at colVals =
  do
    v <- inputAttributeValue at colVals
    return $ atConPlainAttr at v

-------------------------------------------------------------------------------
inputAttributeValue :: INPUT_FOR_EXISTING atConf
                    => AttributeType atConf dbTable typeForExisting typeForCreate
                    -> [SqlValue]
                    -> TranslationMonad typeForExisting
inputAttributeValue at colVals =
  case atInputerExisting at colVals of
    Left e ->
      let
        msg = atCrossRefKey at
      in
       throwError $ AttributeTranslationError msg e
    Right v -> return v
