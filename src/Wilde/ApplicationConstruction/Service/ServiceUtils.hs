-------------------------------------------------------------------------------
-- | Utilities for implementing services.
-------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ApplicationConstruction.Service.ServiceUtils
       (
         -- * Service Titles

         TwoStepServiceTitles(..),
         twoStepServiceTitlesWithSameStyle,

         -- * Utilities for standard services

         showOneComponent,
         showOnePage,
         showOnePageService,

         deleteOnePage,

         -- * Useful actions on 'Object's and 'ObjectType's

         createObject,
         createObjectAny,

         updateObject,
         deleteObject,

         insertAndSelect,
         insertAndSelectAny,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Either

import qualified Data.List.NonEmpty as NonEmpty

import qualified Wilde.Media.ElementSet as ElementSet
import           Wilde.Media.WildeMedia
import qualified Wilde.Media.Database as DbM
import qualified Wilde.Media.Database.Monad as DbConn
import qualified Wilde.Media.Presentation as Presentation
import           Wilde.Media.UserInteraction.Io as UiIo
import qualified Wilde.Media.UserInteraction.Input as UiI

import           Wilde.ObjectModel.ObjectModelUtils
import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.UserInteraction

import qualified Wilde.ObjectModel.Database.Execution.SelectWithPresentationInfo as InputWithPresentation
import qualified Wilde.ObjectModel.Database.Execution.Delete as DbExDelete
import qualified Wilde.ObjectModel.Database.Execution.Insert as DbExInsert
import qualified Wilde.ObjectModel.Database.Execution.Update as DbExUpdate
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import qualified Wilde.ObjectModel.UserInteraction.Input.ForCreate   as InputForCreate
import qualified Wilde.ObjectModel.UserInteraction.Input.ForExisting as InputForExisting
import qualified Wilde.ObjectModel.Presentation as Presentation

import           Wilde.Service.Monad as Service
import           Wilde.Application.Service.Result
import           Wilde.WildeUi.UiPrimitives

import qualified Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectComponent as ObjComp


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Types and titles -
-------------------------------------------------------------------------------


-- | Page titles for a service with two steps.
data TwoStepServiceTitles =
  TwoStepServiceTitles
  {
    page1Title :: WildeTitle
  , page2Title :: WildeTitle
  }

twoStepServiceTitlesWithSameStyle :: WildeStyle
                                  -> Title
                                  -> Title
                                  -> TwoStepServiceTitles
twoStepServiceTitlesWithSameStyle style title1 title2 =
  TwoStepServiceTitles
  {
    page1Title = wildeStyling style title1
  , page2Title = wildeStyling style title2
   }

-- | Gives a component showing given attributes (in given order) of
-- the given object.
showOneComponent
  :: Presentation.ATTRIBUTE_PRESENTATION atConf
  => [Any (AttributeType atConf dbTable)]
  -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
  -> Presentation.Monad AnyCOMPONENT
showOneComponent attributeTypesOrder o =
  do
    atListSetup <- Presentation.toPresentationMonad $
                   AttributeTypeListSetup.mkGeneral (oType o) attributeTypesOrder
    let getAttrs = AttributeTypeListSetup.apply atListSetup
    ObjComp.getShowOneComponent getAttrs o

showOnePage :: Presentation.ATTRIBUTE_PRESENTATION atConf
            => [Any (AttributeType atConf dbTable)]
            -> WildeTitle
            -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
            -> Presentation.Monad ServicePage
showOnePage attributeTypesOrder title o =
  do
    anyComponent <- showOneComponent attributeTypesOrder o
    pure  (title,[anyComponent])

showOnePageService :: Presentation.ATTRIBUTE_PRESENTATION atConf
                   => [Any (AttributeType atConf dbTable)]
                   -- ^ Specifies the order in which
                   -- the 'Attribute's inputers are listed.
                   -> WildeTitle
                   -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
                   -> ServiceMonad ServicePage
showOnePageService attributeTypesOrder title o =
  toServiceMonad $ showOnePage attributeTypesOrder title o


-------------------------------------------------------------------------------
-- - Delete -
-------------------------------------------------------------------------------


deleteOnePage :: ObjectType otConf atConf dbTable otN idAE idAC -> WildeTitle
              -> ServicePage
deleteOnePage ot title = (title, [])


-------------------------------------------------------------------------------
-- - Create -
-------------------------------------------------------------------------------


-- | Inputs an 'ObjectForCreate' from the User Interaction media,
-- inserts the object into the database and reads it from the
-- database including presentation info and pures it.
createObject :: (Database.OBJECT_TYPE_INSERT otConf
                ,Database.DATABASE_IO atConf
                ,InputForCreate.ATTRIBUTE_INPUT_FOR_CREATE atConf
                ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf)
             => ObjectType otConf atConf dbTable oNative idAtE idAtC
             -> ObjectName
             -> ServiceMonad (ObjectInputResult
                              (Object otConf atConf dbTable oNative idAtE idAtC))
createObject ot oName =
  do
    oForCreateR <- toServiceMonad $ InputForCreate.inputer ot oName
    case oForCreateR of
      Left err -> pure $ Left err
      Right oForCreate ->
        let myInsertAndSelect = do
                o <- insertAndSelect oForCreate
                pure $ Right o
        in  Service.toServiceMonad_wDefaultDbConn $ DbConn.inTransaction myInsertAndSelect

-- | See 'createObject'.
createObjectAny :: (Database.OBJECT_TYPE_INSERT otConf
                   ,Database.DATABASE_IO atConf
                   ,InputForCreate.ATTRIBUTE_INPUT_FOR_CREATE atConf
                   ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf)
                => AnyO (ObjectType otConf atConf)
                -> ObjectName
                -> ServiceMonad (ObjectInputResult (AnyO (Object otConf atConf)))
createObjectAny anyOt oName = toAnyForOtAndArg createObject (oName,anyOt)


-------------------------------------------------------------------------------
-- - Update -
-------------------------------------------------------------------------------


-- | Inputs the 'Attribute's of the given 'ObjectType' that are updatable
-- from the User Interaction media,
-- updates the 'Object' in the database, reads it from the
-- database including presentation info and pures it.
updateObject :: forall otConf atConf dbTable oNative idAtE idAtC.(Database.DATABASE_TABLE otConf
                ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                ,InputForExisting.ATTRIBUTE_INPUT_FOR_EXISTING atConf
                ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                )
             => ObjectType otConf atConf dbTable oNative idAtE idAtC
             -> NonEmpty.NonEmpty (Any (AttributeType atConf dbTable))
             -> (ObjectName,idAtE) -- ^ (name,primary key)
             -> ServiceMonad (ObjectInputResult
                              (Object otConf atConf dbTable oNative idAtE idAtC))
updateObject ot updatableAts (oName,pk) =
  do
    attrsToUpdateResult <- inputAttributes updatableAts
    either
      theErrorAsItIs
      (updateInDbAndpureObjectReadFromDb pk)
      attrsToUpdateResult
    where
      theErrorAsItIs err = pure $ Left err

      updateInDbAndpureObjectReadFromDb :: idAtE
                                          -> NonEmpty.NonEmpty (Any (Attribute atConf dbTable))
                                          -> ServiceMonad (ObjectInputResult
                                                           (Object otConf atConf dbTable oNative idAtE idAtC))
      updateInDbAndpureObjectReadFromDb pk attrs =
        Service.toServiceMonad_wDefaultDbConn $ DbConn.inTransaction $ do
          o <- updateAndSelect ot pk attrs
          pure $ Right o

      inputAttributes :: NonEmpty.NonEmpty (Any (AttributeType atConf dbTable))
                      -> ServiceMonad (ObjectInputResult (NonEmpty.NonEmpty (Any (Attribute atConf dbTable))))
      inputAttributes ats =
        do
          attrs <- toServiceMonad $ mapM inputAttribute (NonEmpty.toList ats)
          let (errors,attrsSuccessfullyInput) = partitionEithers $ attrs
          case errors of
            []     -> pure $ pure attrsSuccessfullyInput_asNonEmpty
              where
                attrsSuccessfullyInput_asNonEmpty =
                  (NonEmpty.:|) (head attrsSuccessfullyInput) (tail attrsSuccessfullyInput)
            (x:xs) -> pure $ Left $ errorInfo $ (NonEmpty.:|) x xs

      errorInfo :: NonEmpty.NonEmpty ElementSet.ElementLookupError
                -> ObjectInputErrorInfo
      errorInfo errors = otUiObjectInputErrorInfo
                         (otCrossRefKey ot)
                         oName
                         errors

      inputAttribute :: Any (AttributeType atConf dbTable)
                     -> UiI.Monad (ElementSet.ElementInputResult (Any (Attribute atConf dbTable)))
      inputAttribute (Any at) =
        do
          res <- InputForExisting.inputAttr oName at
          pure $ fmap Any res


-------------------------------------------------------------------------------
-- - Delete -
-------------------------------------------------------------------------------


-- | Deletes the 'Object' in the database.
deleteObject :: (Database.DATABASE_TABLE otConf
                ,Database.IO_FOR_EXISTING atConf
                ,Database.COLUMN_NAMES atConf)
             => ObjectType otConf atConf dbTable oNative idAtE idAtC
             -> idAtE -- ^ primary key
             -> ServiceMonad (Maybe Integer)
deleteObject ot pk =
      Service.toServiceMonad_wDefaultDbConn $ DbConn.inTransaction $
      DbExDelete.deleteOne ot pk


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


insertAndSelectAny :: (Database.OBJECT_TYPE_INSERT otConf
                      ,Database.DATABASE_IO atConf
                      ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                      )
                   => AnyO (ObjectForCreate otConf atConf)
                   -> DbConn.Monad (AnyO (Object otConf atConf))
insertAndSelectAny (AnyO ofc) = AnyO <$> insertAndSelect ofc

insertAndSelect :: (Database.OBJECT_TYPE_INSERT otConf
                   ,Database.DATABASE_IO atConf
                   ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf)
                => ObjectForCreate otConf atConf dbTable oNative idAE idAC
                -> DbConn.Monad (Object otConf atConf dbTable oNative idAE idAC)
insertAndSelect ofc =
  do
    idAtValue <- DbExInsert.insertOneGetId ofc
    mbObject  <- InputWithPresentation.inputOne (ofcType ofc) idAtValue
    let errMsg = "Just inserted an object, but did not get one when trying to get it from the DB"
    maybe (DbConn.throwErr (DbM.DbUnclassifiedError errMsg)) pure mbObject

-- | Updates one 'Object' in the database and then inputs the updated 'Object' including
-- presentation information.
updateAndSelect :: (Database.DATABASE_TABLE otConf
                   ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                   ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                   )
                => ObjectType otConf atConf dbTable oNative idAE idAC
                -> idAE
                -> NonEmpty.NonEmpty (Any (Attribute atConf dbTable))
                -> DbConn.Monad (Object otConf atConf dbTable oNative idAE idAC)
updateAndSelect ot pk attrsToUpdate =
  do
    DbExUpdate.updateOne_attributes ot pk attrsToUpdate
    mbObject  <- InputWithPresentation.inputOne ot pk
    let errMsg = "Just updated an object, but did not get one when trying to get it from the DB"
    maybe (DbConn.throwErr (DbM.DbUnclassifiedError errMsg)) pure mbObject
