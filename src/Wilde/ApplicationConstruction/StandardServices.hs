-------------------------------------------------------------------------------
-- | Constructs the \"standard services\" of an 'ObjectType' given
-- a 'ObjectTypeSetup'.
--
-- The \"standard services\" are
--
-- * Show One Object
-- * Show Many Objects
-- * Show a Selection of Objects, given a SQL expression that filter 'Object's.
-- * Create One Object
-- * Create One Object from another Object, using the other Object as default values
-- * Update One Object
-- * Delete One Object
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.StandardServices
       (
         mkSetupUpdateOne, -- DEBUG

         module Wilde.Application.ApplicationServices,
         module Wilde.Application.StandardServices,

         ObjectDependentComponent,

         -- * ObjectTypeSetup

         ObjectTypeSetup(..),
         objectTypes,

         -- * Construction of the Standard Services

         standardServices,

         singleObjectAttributeTypeOrder,

         -- * Names of the Standard Services

         -- * Service references

         newObjectTypeServiceReference,
         newObjectServiceReference,

         -- * Link Buttons

         newObjectServiceLinkButtonConstructor,

         createOneLinkButton,
         createOneLinkButtonWithFixedAttributeTypes,

         -- * Utilities

         NEW_LINK_RENDERER(..),
         rendererLinkFor,
         linkRendererFor,

         HAS_SERVICE_NAME(..),
         HAS_SERVICE_TITLE(..),
         --standardServiceTitle,
         serviceTitleAndStyle,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Maybe
import qualified Data.Map as Map

import           Wilde.Utils.ListUtils

import qualified Wilde.Media.Element as Element
import           Wilde.Media.WildeValue

import           Wilde.ObjectModel.ObjectModelUtils
import qualified Wilde.Media.Presentation as Presentation
import qualified Wilde.ObjectModel.UserInteraction as OmUi

import qualified Wilde.ObjectModel.UserInteraction                      as UserInteraction
import qualified Wilde.ObjectModel.UserInteraction.Common               as UiCommon
import qualified Wilde.ObjectModel.UserInteraction.Output.ForExisting   as OutputForExisting
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr

import           Wilde.Service.ServiceLink (ServiceName)

import           Wilde.Application.StandardServices
import           Wilde.Application.ObjectTypeService (OtServiceOtSetup(..))
import           Wilde.Application.ApplicationServices
import qualified Wilde.Service.ServiceLink as ServiceLink
import qualified Wilde.Application.ObjectModelServiceLink as OmServiceLink

import           Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS
import           Wilde.ApplicationConstruction.UserInteraction.Output.ObjectDependentComponent
import qualified Wilde.ApplicationConstruction.Service.ServiceUtils as SU

import           Wilde.ApplicationConstruction.StandardServices.SingleObjectServiceCommon
import qualified Wilde.ApplicationConstruction.StandardServices.ShowManyUtils as ShowManyUtils
import qualified Wilde.ApplicationConstruction.StandardServices.ShowMany      as ShowMany
import qualified Wilde.ApplicationConstruction.StandardServices.ShowSelection as ShowSelection
import qualified Wilde.ApplicationConstruction.StandardServices.ShowOne       as ShowOne
import qualified Wilde.ApplicationConstruction.StandardServices.CreateOne     as CreateOne
import qualified Wilde.ApplicationConstruction.StandardServices.CreateOneFrom as CreateOneFrom
import qualified Wilde.ApplicationConstruction.StandardServices.UpdateOne     as UpdateOne
import qualified Wilde.ApplicationConstruction.StandardServices.DeleteOne     as DeleteOne


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Service Names -
-------------------------------------------------------------------------------


-- | Class for making it possible to overload 'serviceName'.
class HAS_SERVICE_NAME a where
  serviceName :: a -> ServiceName

instance HAS_SERVICE_NAME StandardObjectTypeServiceEnum where
  serviceName x = case x of
    CreateOne     -> "CreateOne"
    ShowMany      -> "ShowMany"
    ShowSelection -> "ShowSelection"

instance HAS_SERVICE_NAME StandardObjectServiceEnum where
  serviceName x = case x of
    CreateOneFrom -> "CreateOneFrom"
    ShowOne       -> "ShowOne"
    UpdateOne     -> "UpdateOne"
    DeleteOne     -> "DeleteOne"

instance HAS_SERVICE_NAME StandardServiceEnum where
  serviceName (StandardObjectTypeService x) = serviceName x
  serviceName (StandardObjectService     x) = serviceName x


-------------------------------------------------------------------------------
-- | Reference to Object Type Service.
--
-- 'ObjectTypeSetup' argument is used only to make it a little bit
-- probable that the service really exist in the application.
-- (Strictly, only the 'ObjectType' is needed as argument.)
--
-- (It is a weakness of this library that it cannot be guaranteed (at compile time)
-- that referenced services really exist in the application.)
-------------------------------------------------------------------------------
newObjectTypeServiceReference :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                              -> StandardObjectTypeServiceEnum
                              -> ServiceLink.ObjectTypeServiceReference
newObjectTypeServiceReference ot serviceEnum =
  OmServiceLink.newObjectTypeServiceReference (serviceName serviceEnum) ot

-------------------------------------------------------------------------------
-- | Reference to Object Service.
--
-- Comments of 'newObjectTypeServiceReference' applies to this function, too.
-------------------------------------------------------------------------------
newObjectServiceReference :: OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                          -> StandardObjectServiceEnum
                          -> ServiceLink.ObjectServiceReference idAtExisting
newObjectServiceReference ot serviceEnum =
  OmServiceLink.newObjectServiceReference (serviceName serviceEnum) ot


-------------------------------------------------------------------------------
-- - ObjectTypeSetup -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Setup of the standard services for a given 'ObjectType'.
--
-- Also, in the future, specifies which standard services exist for
-- a given 'ObjectType'.
--
-- Currently, all standard services always exist - there is no way to
-- specify a subset.
-------------------------------------------------------------------------------
data ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate =
  ObjectTypeSetup
  {
    objectType          :: ObjectType          otConf atConf dbTable otNative idAtExisting idAtCreate
  , titleWithStyle      :: StyledTitle
  , objectListSetup     :: OLS.ObjectListSetup otConf atConf dbTable otNative idAtExisting idAtCreate
    -- | The order of the attributes used for single-object presentation/ui.
    -- If null (empty), the order of the attributes in 'ObjectType' is used.
    -- If not null (non-empty): the list must be a permutation of all
    -- attributes in the 'ObjectType'.
  , alternativeAtsOrder :: [Any (AttributeType atConf dbTable)]
    -- | Dependent components for "Show One".
  , dependentComponents :: [ObjectDependentComponent  otConf atConf dbTable otNative idAtExisting idAtCreate]
    -- | If there is no 'AttributeType' set for user input
    -- ('OutputForExisting.UserInput'), then the object type will not
    -- have an update service.
  , updateConfig        :: [OutputForExisting.AttributeTypeConfiguration atConf dbTable]
  , deleteSteps         :: DeleteOne.Steps idAtExisting
  }

-------------------------------------------------------------------------------
-- - Construction of Standard Services -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Constructs the \"standard services\" for a list of 'ObjectType's.
-------------------------------------------------------------------------------
standardServices :: (Database.OBJECT_TYPE_INSERT otConf
                    ,Database.DATABASE_IO atConf
                    ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                    ,UserInteraction.ATTRIBUTE_IO_FOR_CREATE atConf
                    ,UserInteraction.ATTRIBUTE_IO_FOR_EXISTING atConf
                    ,OmGsr.ATTRIBUTE_IO_FOR_EXISTING atConf
                    )
                 => [AnyO (ObjectTypeSetup otConf atConf)]
                 -> ApplicationServices
standardServices otss =
  Map.fromList
  (
    map csServiceNameAndImpl
    [ (ShowMany          ,ShowMany.mkService      showManySetups)
    , (ShowSelection     ,ShowSelection.mkService showManySetups)
    , (CreateOne         ,CreateOne.mkService     createOneSetups)
    ]
    ++
    map csServiceNameAndImpl
    [ (ShowOne           ,ShowOne.mkService       showOneSetups)
    , (CreateOneFrom     ,CreateOneFrom.mkService createOneFromSetups)
    , (UpdateOne         ,UpdateOne.mkService     updateOneSetups)
    , (DeleteOne         ,DeleteOne.mkService     deleteOneSetups)
    ]
  )
  where

    csServiceNameAndImpl :: HAS_SERVICE_NAME a
                         => (a,AnyOtService)
                         -> (ServiceName,ApplicationService)
    csServiceNameAndImpl (serviceNameEnum,otService) =
      (serviceName serviceNameEnum, mkObjectTypeService otService)

    showManySetups      = map      (anyOApply2 mkSetupShowMany)        otss
    createOneSetups     = map      (anyOApply2 mkSetupCreateOne)       otss
    showOneSetups       = map      (anyOApply2 mkSetupShowOne)         otss
    createOneFromSetups = map      (anyOApply2 mkSetupCreateOneFrom)   otss
    updateOneSetups     = mapMaybe (anyOApply2_maybe mkSetupUpdateOne) otss
    deleteOneSetups     = map      (anyOApply2 mkSetupDeleteOne)       otss

objectTypes :: [AnyO (ObjectTypeSetup otConf atConf)] -> [AnyO (ObjectType otConf atConf)]
objectTypes = map (anyOApply2 objectType)

mkSetupShowOne :: OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
               => ObjectTypeSetup                 otConf atConf dbTable otNative idAtExisting idAtCreate
               -> OtServiceOtSetup ShowOne.Config otConf atConf dbTable otNative idAtExisting idAtCreate
mkSetupShowOne ots@(ObjectTypeSetup
                    {
                      objectType          = ot
                    , titleWithStyle      = tws
                    , dependentComponents = depComps
                    }) =
    OtServiceOtSetup ot config
  where
    config = ShowOne.Config
             {
               ShowOne.title               = serviceTitleAndStyle ShowOne tws
             , ShowOne.attributeTypesOrder = attributeTypesOrder
             , ShowOne.dependentComponents = depComps
             , ShowOne.buttons             = buttonConstructors
             }
    attributeTypesOrder = singleObjectAttributeTypeOrder ots

    buttonConstructors = map mkObjectServiceLinkButtonCon services

    mkObjectServiceLinkButtonCon oServiceEnum =
      newObjectServiceLinkButtonConstructor ot oServiceEnum

    services :: [StandardObjectServiceEnum]
    services = [ UpdateOne
               , CreateOneFrom
               , DeleteOne
               ]

mkSetupShowMany :: ObjectTypeSetup                  otConf atConf dbTable otNative idAtExisting idAtCreate
                -> OtServiceOtSetup ShowMany.Config otConf atConf dbTable otNative idAtExisting idAtCreate
mkSetupShowMany (ObjectTypeSetup
                {
                  objectType      = ot
                , titleWithStyle  = tws
                , objectListSetup = ols
                }) =
  OtServiceOtSetup ot config
  where
    config = ShowManyUtils.Config
             {
               ShowManyUtils.title           = serviceTitleAndStyle ShowMany tws
             , ShowManyUtils.objectListSetup = ols
             }

mkSetupCreateOne :: ObjectTypeSetup                   otConf atConf dbTable otNative idAtExisting idAtCreate
                 -> OtServiceOtSetup CreateOne.Config otConf atConf dbTable otNative idAtExisting idAtCreate
mkSetupCreateOne ots@(ObjectTypeSetup
                      {
                        objectType     = ot
                      , titleWithStyle = tws
                      }) =
  OtServiceOtSetup ot config
  where
    config = CreateOne.Config
             {
               CreateOne.titles              = twoTitles CreateOne tws
             , CreateOne.attributeTypesOrder = singleObjectAttributeTypeOrder ots
             }

mkSetupCreateOneFrom :: ObjectTypeSetup                       otConf atConf dbTable otNative idAtExisting idAtCreate
                     -> OtServiceOtSetup CreateOneFrom.Config otConf atConf dbTable otNative idAtExisting idAtCreate
mkSetupCreateOneFrom ots@(ObjectTypeSetup
                          {
                            objectType     = ot
                          , titleWithStyle = tws
                          }) =
  OtServiceOtSetup ot config
  where
    config = CreateOneFrom.Config
             {
               CreateOneFrom.titles              = twoTitles CreateOneFrom tws
             , CreateOneFrom.attributeTypesOrder = singleObjectAttributeTypeOrder ots
             }

mkSetupUpdateOne :: ObjectTypeSetup                          otConf atConf dbTable otNative idAtExisting idAtCreate
                 -> Maybe (OtServiceOtSetup UpdateOne.Config otConf atConf dbTable otNative idAtExisting idAtCreate)
mkSetupUpdateOne ots@(ObjectTypeSetup
                      {
                        objectType     = ot
                      , titleWithStyle = tws
                      , updateConfig   = theUpdateConfig
                      })
  =
  fmap
  aConfig
  (UpdateOne.updatableAts theUpdateConfig)
  where
    aConfig theUpdatables =
                  OtServiceOtSetup ot $
                  UpdateOne.Config
                  {
                    UpdateOne.titles                     = twoTitles UpdateOne tws
                  , UpdateOne.displayUpdatedObjectConfig = singleObjectAttributeTypeOrder ots
                  , UpdateOne.inputFormConfig            = theUpdateConfig
                  , UpdateOne.updatables                 = theUpdatables
                  }

mkSetupDeleteOne :: ObjectTypeSetup                   otConf atConf dbTable otNative idAtExisting idAtCreate
                 -> OtServiceOtSetup DeleteOne.Config otConf atConf dbTable otNative idAtExisting idAtCreate
mkSetupDeleteOne (ObjectTypeSetup
                  {
                    objectType     = ot
                  , titleWithStyle = tws
                  , deleteSteps    = theDeleteSteps
                  }) =
  OtServiceOtSetup ot config
  where
    config = DeleteOne.Config
             {
               DeleteOne.title = serviceTitleAndStyle DeleteOne tws
             , DeleteOne.steps = theDeleteSteps
             }


-------------------------------------------------------------------------------
-- | Buttons that are links to the \"standard services\".
-------------------------------------------------------------------------------


-- | Constructs a UI button for one of the stanard Object Services.
newObjectServiceLinkButtonConstructor :: OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                                      => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                                      -> StandardObjectServiceEnum
                                      -> Presentation.Monad (idAtExisting -> AnySVALUE)
newObjectServiceLinkButtonConstructor ot@(ObjectType {}) oServiceEnum =
  objectServiceLinkButton oServiceEnum serviceReference
  where
    serviceReference = newObjectServiceReference ot oServiceEnum

-- | A button for an "Object Service" - a Service that operates on an existing object.
objectServiceLinkButton :: (Typeable idAtExisting, Show idAtExisting)
                        => StandardObjectServiceEnum
                        -> ServiceLink.ObjectServiceReference idAtExisting
                        -> Presentation.Monad (idAtExisting -> AnySVALUE)
objectServiceLinkButton oServiceEnum objSrvcRef =
  do
    srvcLinkCon   <- ServiceLink.getServiceLinkConstructor
    linkRenderer  <- linkRendererFor oServiceEnum
    pure $ \pk ->
      let srvcLink = srvcLinkCon (objSrvcRef pk)
      in  linkRenderer srvcLink

createOneLinkButton :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                    -> Presentation.Monad AnySVALUE
createOneLinkButton ot =
  createOneLinkButtonWithFixedAttributeTypes ot []

createOneLinkButtonWithFixedAttributeTypes :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                                           -> [OmUi.AttributeFixedValue atConf dbTable]
                                           -> Presentation.Monad AnySVALUE
createOneLinkButtonWithFixedAttributeTypes ot fixedAts =
  do
    serviceLinkCon        <- ServiceLink.getServiceLinkConstructor
    let serviceLinkNoFixed = serviceLinkCon createOneServiceReference
                             :: ServiceLink.ServiceLink
    let serviceLink        = ServiceLink.addElementParams serviceLinkNoFixed fixParams
    rendererLinkFor CreateOne serviceLink
  where
    createOneServiceReference :: ServiceLink.ServiceReferenceWithParams
    createOneServiceReference = newObjectTypeServiceReference ot CreateOne ()

    fixParams   = concatMap mkFixParam fixedAts :: [Element.Element]
    mkFixParam :: OmUi.AttributeFixedValue atConf dbTable
               -> [Element.Element]
    mkFixParam (OmUi.AttributeFixedValue at_any gsrValue) =
      UiCommon.metaValuesForRole
      UiCommon.Fix
      (atCrossRefKey_anyValue at_any)
      theObjectName
      gsrValue


-------------------------------------------------------------------------------
-- - Order of AttributeType:s -
-------------------------------------------------------------------------------


singleObjectAttributeTypeOrder :: ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
                               -> [Any (AttributeType atConf dbTable)]
singleObjectAttributeTypeOrder (ObjectTypeSetup
                          {
                            objectType          = ot
                          , alternativeAtsOrder = alternativeOrder
                          }) =
  firstNonNull (otAttributeTypes ot) [alternativeOrder]


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - NEW_LINK_RENDERER -
-------------------------------------------------------------------------------


-- | Convenience function for constructing a
-- 'ServiceLinkRenderer' for different kinds of standard service enums.
class NEW_LINK_RENDERER srvcEnum where
  newLinkRenderer :: srvcEnum -> StandardServiceLinkRenderer -> ServiceLinkRenderer

instance NEW_LINK_RENDERER StandardObjectTypeServiceEnum where
  newLinkRenderer otSrvcEnum standardServiceLinkRenderer =
    standardServiceLinkRenderer (StandardObjectTypeService otSrvcEnum)

instance NEW_LINK_RENDERER StandardObjectServiceEnum where
  newLinkRenderer otSrvcEnum standardServiceLinkRenderer =
    standardServiceLinkRenderer (StandardObjectService otSrvcEnum)

instance NEW_LINK_RENDERER StandardServiceEnum where
  newLinkRenderer srvcEnum standardServiceLinkRenderer =
    standardServiceLinkRenderer srvcEnum


-------------------------------------------------------------------------------
-- - HAS_SERVICE_TITLE -
-------------------------------------------------------------------------------


class HAS_SERVICE_TITLE a where
  serviceTitle :: a -> Title -> String

instance HAS_SERVICE_TITLE StandardObjectTypeServiceEnum where
  serviceTitle x = case x of
    CreateOne     -> (++ " *")
    ShowMany      -> (++ " ...")
    ShowSelection -> (++ " ...")

instance HAS_SERVICE_TITLE StandardObjectServiceEnum where
  serviceTitle x = case x of
    CreateOneFrom -> (++ " *")
    UpdateOne     -> (++ " ~")
    DeleteOne     -> (++ " -")
    ShowOne       -> id

instance HAS_SERVICE_TITLE StandardServiceEnum where
  serviceTitle (StandardObjectTypeService x) = serviceTitle x
  serviceTitle (StandardObjectService     x) = serviceTitle x


-------------------------------------------------------------------------------
-- - Rendering links -
-------------------------------------------------------------------------------


rendererLinkFor :: NEW_LINK_RENDERER a
                => a
                -> ServiceLink.ServiceLink
                -> Presentation.Monad AnySVALUE
rendererLinkFor serviceSpec serviceLink =
  do
    linkRenderer <- linkRendererFor serviceSpec
    pure $ linkRenderer serviceLink

linkRendererFor :: NEW_LINK_RENDERER a
                => a
                -> Presentation.Monad ServiceLinkRenderer
linkRendererFor serviceSpec =
  do
    standardServiceLinkRenderer <- Presentation.getEnvs
                                   Presentation.envStandardServiceLinkRenderer
    pure $ newLinkRenderer serviceSpec standardServiceLinkRenderer


-------------------------------------------------------------------------------
-- - Page titles -
-------------------------------------------------------------------------------


-- | Constructs the standard title and style for a given
-- standard service and 'ObjectType'.
serviceTitleAndStyle :: HAS_SERVICE_TITLE a
                     => a
                     -> StyledTitle
                     -> StyledTitle
serviceTitleAndStyle service title =
  wildeStyling
  style
  (serviceTitle service titleString)
  where
    titleString = wildeStyled title
    style       = wildeStyle  title


-- | First title is transformed for the 'StandardServiceEnum', second is
-- identical to the given title.
--
-- Given title is supposed to be the "name" of the object type.
twoTitles :: HAS_SERVICE_TITLE a
          => a
          -> StyledTitle
          -> SU.TwoStepServiceTitles
twoTitles serviceEnum objectTitle =
  SU.TwoStepServiceTitles
  {
    SU.page1Title = objectTitle
                    `withAdjustedStyled`
                    (serviceTitle serviceEnum)
  , SU.page2Title = objectTitle
  }
  where
    objectTypeName = wildeStyled objectTitle
    style          = wildeStyle  objectTitle
