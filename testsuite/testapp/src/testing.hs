module Testing where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.NonEmptyList as NonEmpty

import qualified Wilde.ObjectModel.UserInteraction.Output.ForExisting as OutputForExisting

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDbConfig

import Wilde.Driver.Application.Cgi.ApplicationMain


import Wilde.ObjectModel.ObjectModel

import Wilde.ApplicationConstruction.StandardServices

import qualified ObjectType.CharacterEncodingExperiment as OT


import Wilde.ApplicationConstruction.ObjectModel.ObjectType

import qualified Wilde.ApplicationConstruction.StandardServices.UpdateOne as UpdateOne

import qualified Wilde.Application.ObjectTypeService as ObjectTypeService


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


ots :: ObjectTypeSetup OtDbConfig.Configuration DdlAtAnnotation.Configuration OT.Table OT.NativeType PrimaryKeyType (Maybe PrimaryKeyType)
ots = OT.ots

otServices :: ApplicationServices
otServices = standardServices $ [AnyO ots]

setupUpdateOne :: ObjectTypeService.OtServiceOtSetup UpdateOne.Config  OtDbConfig.Configuration DdlAtAnnotation.Configuration OT.Table OT.NativeType PrimaryKeyType (Maybe PrimaryKeyType)
setupUpdateOne = case mkSetupUpdateOne ots of
  Nothing -> error "går inte att uppdatera"
  Just x  -> x


updateOneConfig
  :: UpdateOne.Config
       OtDbConfig.Configuration
       DdlAtAnnotation.Configuration
       OT.Table
       OT.NativeType
       PrimaryKeyType
       (Maybe PrimaryKeyType)
updateOneConfig = ObjectTypeService.setupConfig setupUpdateOne


confs
  :: [UpdateOne.AttributeTypeConfiguration DdlAtAnnotation.Configuration OT.Table]
confs = UpdateOne.inputFormConfig $! updateOneConfig

uats = OutputForExisting.atsWith OutputForExisting.UserInteraction confs

mbUats = UpdateOne.updatableAts confs
