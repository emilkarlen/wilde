-------------------------------------------------------------------------------
-- | Tools for constructing the 'AttributeType's of an application.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.ObjectModel.AttributeType
       (
         -- * Re-exporting common closely related modules

         module Database.HDBC.Types,
         module Database.HDBC.ColTypes,

         module Wilde.WildeUi.WildeStyle,
         module Wilde.ObjectModel.ObjectModel,

         module Wilde.WildeUi.WildeValue,
         module Wilde.WildeUi.StdValueTypes,

         -- * Types from other modules

         Data.Time.Calendar.Day,
         Word32,
         Int32,

         -- * Type synonyms

         PrimaryKeyType,

         AttributeType_ddl,

         PlainAttributeType,
         PlainAttributeType_optional,
         PlainAttributeType_ddl,
         PlainAttributeType_optional_ddl,

         StdPkAttributeType,
         StdPkAttributeType_ddl,

         StdPkAttributeType_optional,
         StdPkAttributeType_optional_ddl,
         StdAutoPkPkAttributeType,
         StdAutoPkPkAttributeType_ddl,

         StdRefAttributeType,
         StdRefAttributeType_optional,

         StdRefAttributeType_ddl,
         StdRefAttributeType_optional_ddl,

         -- * Utilities

         -- ** Default values

         noDefault,
         defaultPreFill,
         defaultValue,

         withDefault,

         -- ** Presentation value constructs

         unquotedDropDownValue,
         htmlBoolChar,

         -- * AttributeType:s

         at_Word32,
         at_Word32_optional,

         at_String,
         at_String_forDefaultDbIo,

         at_String_optional,
         at_String_optional_forDefaultDbIo,

         at_Text,
         at_Text_optional,

         at_Text_html_optional,

         at_Href,
         at_Href_optional,

         at_Double_withExprUiInput,
         at_Double_optional_withExprUiInput,

         at_Date,
         at_Date_optional,

         at_Date_withConvenienteUiInput,
         at_Date_optional_withConvenienteUiInput,

         at_PrimaryKeyType,
         at_PrimaryKeyType_optional,
         at_PrimaryKey,
         at_PrimaryKey_dbAutogen,

         at_ref,
         at_ref_optional,

         at_ref_std,
         at_ref_std_optional,

         at_EC,

         at_Bool,
         at_Bool_checkBox,
         at_Bool_optional_dropDown,
         at_Bool_Word32,
         at_Bool_Word32_optional,

         at_EnumAsDropDown_Word32,
         at_EnumAsDropDown_Word32_optional,

         at_EnumAsDropDown_mandatory,
         at_EnumAsDropDown_optional,

         at_GenericWidgetDefaultValue,
         at_GenericWidgetDefaultValue_optional,

         atGsrIo_PrimaryKey,
         atGsrIo_PrimaryKey_optionalOnCreate,

         atDbConfE_PrimaryKeyType,
         atDbConfE_PrimaryKeyType_optional,

         pres_PrimaryKeyType,

         atInfoForCreate,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.Convertible.Base

import           Data.Word
import           Data.Int

import           Data.Time.Calendar (Day)

import           Database.HDBC.Types
import           Database.HDBC.ColTypes

import qualified Data.List.NonEmpty as NonEmpty
import           Wilde.Utils.Empty

import           Wilde.Database.Sql
import           Wilde.Database.SqlDdlInfo

import           Wilde.WildeUi.WildeValue
import           Wilde.WildeUi.UiPrimitives (Title, WildeTitle)
import           Wilde.WildeUi.WildeStyle
import           Wilde.WildeUi.StdValueTypes

import           Wilde.Media.GenericStringRep
import qualified Wilde.Media.UserInteraction.Input as UiI
import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.UserInteraction.Io as UiIo
import           Wilde.Media.ElementSet
import           Wilde.Media.Database

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.UserInteraction as UserInteraction
import           Wilde.ObjectModel.DatabaseAndPresentation (AttributeWithPresentationInfoDbInputerInfo(..))
import           Wilde.ObjectModel.UserInteraction.Output.CreateCommon

import           Wilde.ApplicationConstruction.Database.DatabaseColumnTypes (dbIo_string_default, dbIo_string_optional_default)
import           Wilde.ApplicationConstruction.StandardServices as StandardServices
import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType
import           Wilde.ApplicationConstruction.GenericStringRepIo
import qualified Wilde.ApplicationConstruction.Database.AttributeTypeDatabaseInfo as AtDbInfo
import qualified Wilde.ApplicationConstruction.ElementSetUtils as ESU
import qualified Wilde.ApplicationConstruction.UserInteraction.Input.UserInteractionInputers as UIIs
import           Wilde.ApplicationConstruction.UserInteraction.Output.LabelAndWidget as LabelAndWidget
import           Wilde.ApplicationConstruction.Presentation.Presentation
import           Wilde.ApplicationConstruction.ObjectModel.StandardPrimaryKey as StandardPrimaryKey
import           Wilde.ApplicationConstruction.UserInteraction.Io
import qualified Wilde.ApplicationConstruction.UserInteraction.Widgets as Widgets
import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import           Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo as UiIoAndDbIo


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


atInfoForCreate :: AttributeTypeUserInteractionIo             typeForExisting typeForCreate
                -> AtDbInfo.AttributeTypeDatabaseInfo dbTable typeForExisting typeForCreate
                -> AttributeTypeMediaIoForCreate              typeForExisting typeForCreate
atInfoForCreate atUiIo atDbInfo =
  AttributeTypeMediaIoForCreate
  {
    aticDatabaseOutputer = AtDbInfo.atdbioCreateOutputer atDbInfo,
    aticUiIo             = atuiioCreateIo atUiIo
  }


-------------------------------------------------------------------------------
-- - Type Synonyms -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - PlainAttributeType -
-------------------------------------------------------------------------------


type PlainA atConf dbTable a = AttributeType atConf dbTable a a

-- | An 'AttributeType' with identical types for an existing value and the
-- value for construction via the database.
type PlainAttributeType     dbTable a = PlainA UiIoAndDbIo.ConfigurationSansAnnotation dbTable a
type PlainAttributeType_ddl dbTable a = PlainA DdlAtAnnotation.Configuration           dbTable a

type AttributeType_ddl      dbTable a = AttributeType DdlAtAnnotation.Configuration    dbTable a


-------------------------------------------------------------------------------
-- - PlainAttributeType_optional -
-------------------------------------------------------------------------------


type PlainA_optional atConf dbTable a = AttributeType atConf dbTable (Maybe a) (Maybe a)

-- | An optional 'AttributeType' with identical types for an existing value and the
-- value for construction via the database.
type PlainAttributeType_optional     dbTable a = PlainA_optional UiIoAndDbIo.ConfigurationSansAnnotation dbTable a
type PlainAttributeType_optional_ddl dbTable a = PlainA_optional DdlAtAnnotation.Configuration           dbTable a

-- | An 'AttributeType' with the standard primary key type.
type StdPkAttributeType     dbTable = PlainAttributeType     dbTable PrimaryKeyType
type StdPkAttributeType_ddl dbTable = PlainAttributeType_ddl dbTable PrimaryKeyType

-- | An 'AttributeType' with optional standard primary key type.
type StdPkAttributeType_optional     dbTable = PlainAttributeType_optional     dbTable PrimaryKeyType
type StdPkAttributeType_optional_ddl dbTable = PlainAttributeType_optional_ddl dbTable PrimaryKeyType

-- | A primary key 'AttributeType' with the standard type of primary key ('PrimaryKeyType').
type StdAutoPkPkAttributeType     dbTable = AttributeType UiIoAndDbIo.ConfigurationSansAnnotation dbTable PrimaryKeyType (Maybe PrimaryKeyType)
type StdAutoPkPkAttributeType_ddl dbTable = AttributeType DdlAtAnnotation.Configuration           dbTable PrimaryKeyType (Maybe PrimaryKeyType)

-- | A mandatory reference 'AttributeType' to an 'StdObjectType'.
type StdRefAttributeType     dbTable = StdPkAttributeType     dbTable
type StdRefAttributeType_ddl dbTable = StdPkAttributeType_ddl dbTable

-- | An optional reference 'AttributeType' to a 'StdObjectType'.
type StdRefAttributeType_optional     dbTable = PlainAttributeType_optional     dbTable PrimaryKeyType
type StdRefAttributeType_optional_ddl dbTable = PlainAttributeType_optional_ddl dbTable PrimaryKeyType

-- | Inputer of a value of type a for an 'AttributeType' with a give name.
type UserInteractionInputerForAttribute a =
  UiIo.AttributeName -> UiI.UserInteractionInputer (ElementInputResult a)


-------------------------------------------------------------------------------
-- - Default values -
-------------------------------------------------------------------------------


noDefault :: Maybe (UserInteraction.AttributeTypeCreateOption a)
noDefault = Nothing

defaultPreFill :: UiIo.GenericWidgetDefaultValue
               -> Maybe (UserInteraction.AttributeTypeCreateOption a)
defaultPreFill = Just . UserInteraction.AtuicoDefault . Left

defaultValue :: a
             -> Maybe (UserInteraction.AttributeTypeCreateOption a)
defaultValue = Just . UserInteraction.AtuicoDefault . Right

withDefault :: AttributeType_ddl dbTable a a
            -> Maybe (UserInteraction.AttributeTypeCreateOption a)
            -> AttributeType_ddl dbTable a a
withDefault at newDefault = at { atConfiguration = newAtConf }
  where
    newAtConf = (atConfiguration at) { UiIoAndDbIo.uiCreateOption = newDefault }


-------------------------------------------------------------------------------
-- - AttributeType constructors -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Primary Key -
-------------------------------------------------------------------------------


-- | An ID attribute, that is given explicitly (i.e. not created automatically
-- by the database when the record is inserted) .
at_PrimaryKey :: SQL_IDENTIFIER dbTable
              => Int
              -> dbTable
              -> StdPkAttributeType_ddl dbTable
at_PrimaryKey inputWidth field = at_Word32 inputWidth field noDefault "ID"

at_PrimaryKeyType :: SQL_IDENTIFIER dbTable
                  => Int
                  -> dbTable
                  -> Maybe (UserInteraction.AttributeTypeCreateOption PrimaryKeyType)
                  -> Title
                  -> StdPkAttributeType_ddl dbTable
at_PrimaryKeyType = at_Word32

at_PrimaryKeyType_optional :: SQL_IDENTIFIER dbTable
                           => Int
                           -> dbTable
                           -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe PrimaryKeyType))
                           -> Title
                           -> StdPkAttributeType_optional_ddl dbTable
at_PrimaryKeyType_optional = at_Word32_optional

-- | An ID attribute, that is automatically generated in the db when
-- inserted.
at_PrimaryKey_dbAutogen :: SQL_IDENTIFIER dbTable
                        => Int
                        -> dbTable
                        -> AttributeType DdlAtAnnotation.Configuration dbTable PrimaryKeyType (Maybe PrimaryKeyType)
at_PrimaryKey_dbAutogen inputWidth field =
   AttributeType
   {
     atCrossRefKey              = attributeName
   , atPresentationO            = presO_PrimaryKeyType
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_PrimaryKeyType "ID"
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_optionalForCreate atGsrIo_convertibleFromInteger_nonEmpty
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Word32 inputWidth)
     , UiIoAndDbIo.uiCreateOption           = Just $ UserInteraction.AtuicoFixed Nothing
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate
                                              (uiIo_PrimaryKeyType_optionalOnCreate inputWidth)
                                              (dbIo_PrimaryKeyType_optionalOnCreate field)
     , UiIoAndDbIo.databaseConfForExisting  = dbConfE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    dbConfE       = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = StandardPrimaryKey.atDbInfo_PrimaryKeyType_optionalOnCreate field


-------------------------------------------------------------------------------
-- - References -
-------------------------------------------------------------------------------


at_ref :: (Database.DATABASE_TABLE otConf
          ,SQL_IDENTIFIER dbTableSrc
          ,SQL_IDENTIFIER dbTableDst
          ,Show e
          ,Typeable e
          )
       => dbTableSrc
       -> ObjectTypeSetup           otConf DdlAtAnnotation.Configuration dbTableDst otNativeDst e c
       -> ReferencePresentationSpec otConf DdlAtAnnotation.Configuration dbTableDst otNativeDst e c
       -> Maybe WildeTitle
       -> PlainAttributeType_ddl dbTableSrc e
at_ref = at_ref' False

at_ref' :: (Database.DATABASE_TABLE otConf
           ,SQL_IDENTIFIER dbTableSrc
           ,SQL_IDENTIFIER dbTableDst
           ,Show e
           ,Typeable e)
        => Bool
        -> dbTableSrc
        -> ObjectTypeSetup           otConf DdlAtAnnotation.Configuration dbTableDst otNativeDst e c
        -> ReferencePresentationSpec otConf DdlAtAnnotation.Configuration dbTableDst otNativeDst e c
        -> Maybe WildeTitle
        -> PlainAttributeType_ddl    dbTableSrc e
at_ref' bOptional field otSetupDst refPresSpec mbTitle =
  reference_mandatory rati setDdlInfoOnDdlAnnotation field title
  where
    setDdlInfoOnDdlAnnotation atConf fieldDst = DdlAtAnnotation.mkDdlAtAnnotation $
                                             NonEmpty.singleton ddlInfo
      where
        ddlInfo = ddlColumnInfoForReference bOptional field otDst ddlColInfoForDstIdAt
    idAtDst                  = otIdAttributeType . objectType $ otSetupDst
    ddlColInfoForDstIdAt     = getSingletonDdlColForRefTarget ddlColInfosForDstIdAt
    ddlColInfosForDstIdAt    = DdlAtAnnotation.atDdlInfo idAtDst
    databaseColumnForDstIdAt = getOtIdAtDbInfo_mustBeSingle (sqlIdentifier field) otDst
    otDst                    = objectType otSetupDst
    title                    = maybe
                               (titleWithStyle otSetupDst
                                `withAdjustedStyle`
                                (const neutral)
                               )
                               id
                               mbTitle
    rati =
      ReferenceAttributeTypeInfo
      {
        ratiUiWidgetConstructor = widget_dropDownList bOptional
      , ratiRefDst              = otSetupDst
      , ratiTableColumns        = NonEmpty.singleton field
      , ratiPresSpec            = refPresSpec
      }

at_ref_optional :: (Database.DATABASE_TABLE otConf
                   ,SQL_IDENTIFIER dbTableSrc
                   ,SQL_IDENTIFIER dbTableDst
                   ,Show typeForExisting
                   ,Typeable typeForExisting)
                => dbTableSrc
                -> ObjectTypeSetup              otConf DdlAtAnnotation.Configuration dbTableDst otNativeDst typeForExisting typeForCreate
                -> ReferencePresentationSpec    otConf DdlAtAnnotation.Configuration dbTableDst otNativeDst typeForExisting typeForCreate
                -> Maybe WildeTitle
                -> PlainAttributeType_optional_ddl dbTableSrc typeForExisting
at_ref_optional field otSetupDst refPresSpec mbTitle =
  reference_optional rati setDdlInfoOnDdlAnnotation field title
  where
    setDdlInfoOnDdlAnnotation atConf fieldDst = DdlAtAnnotation.mkDdlAtAnnotation $
                                             NonEmpty.singleton ddlInfo
      where
        ddlInfo = ddlColumnInfoForReference True field otDst ddlColInfoForDstIdAt
    ddlColInfoForDstIdAt     = getSingletonDdlColForRefTarget ddlColInfosForDstIdAt
    ddlColInfosForDstIdAt    = (DdlAtAnnotation.atDdlInfo . otIdAttributeType . objectType) otSetupDst
    databaseColumnForDstIdAt = getOtIdAtDbInfo_mustBeSingle (sqlIdentifier field) otDst
    otDst                    = objectType otSetupDst
    title                    = maybe
                               (titleWithStyle otSetupDst
                                `withAdjustedStyle`
                                (const neutral)
                               )
                               id
                               mbTitle
    rati = ReferenceAttributeTypeInfo
            {
              ratiUiWidgetConstructor = widget_dropDownList True
            , ratiRefDst              = otSetupDst
            , ratiTableColumns        = NonEmpty.singleton field
            , ratiPresSpec            = refPresSpec
            }

at_ref_std :: (Database.DATABASE_TABLE otConf
              ,SQL_IDENTIFIER dbTableSrc
              ,SQL_IDENTIFIER dbTableDst)
           => dbTableSrc
           -> ObjectTypeSetup           otConf DdlAtAnnotation.Configuration dbTableDst otNativeDst PrimaryKeyType typeForCreate
           -> ReferencePresentationSpec otConf DdlAtAnnotation.Configuration dbTableDst otNativeDst PrimaryKeyType typeForCreate
           -> Maybe WildeTitle
           -> StdRefAttributeType_ddl dbTableSrc
at_ref_std field otSetupDst refPresSpec mbTitle =
  at_ref' False field otSetupDst refPresSpec mbTitle

at_ref_std_optional :: (Database.DATABASE_TABLE otConf
                       ,SQL_IDENTIFIER dbTableSrc
                       ,SQL_IDENTIFIER dbTableDst)
                    => dbTableSrc
                    -> ObjectTypeSetup              otConf DdlAtAnnotation.Configuration dbTableDst otNativeDst PrimaryKeyType typeForCreate
                    -> ReferencePresentationSpec    otConf DdlAtAnnotation.Configuration dbTableDst otNativeDst PrimaryKeyType typeForCreate
                    -> Maybe WildeTitle
                    -> StdRefAttributeType_optional_ddl dbTableSrc
at_ref_std_optional field otSetupDst refPresSpec mbTitle =
  at_ref_optional field otSetupDst refPresSpec mbTitle

getOtIdAtDbInfo_mustBeSingle :: Database.COLUMN_NAMES atConf
                             => SqlIdentifier
                             -> ObjectType     otConf atConf dbTable otNative idAtE idAtC
                             -> DatabaseColumn dbTable
getOtIdAtDbInfo_mustBeSingle refColForErrMsg otDst =
  case getOtIdAtDbInfos otDst of
    [x] -> x
    _ -> error $ errMsgHead ++ errMsgDescription
      where
        errMsgHead        = "at_ref_std...: " ++ refColForErrMsg ++
                            " (destination ObjectType = " ++ otCrossRefKey otDst  ++ ")"
        errMsgDescription = ": ID AttributeType of destination ObjectType has more than one database columns"
  where
    getOtIdAtDbInfos = NonEmpty.toList . Database.atColumns . otIdAttributeType

getOtIdAtDdlColumnInfo_mustBeSingle :: SqlIdentifier
                                    -> ObjectType otConf DdlAtAnnotation.Configuration dbTable otNative idAtE idAtC
                                    -> DdlColumnInfo                  dbTable
getOtIdAtDdlColumnInfo_mustBeSingle refColForErrMsg otDst =
  case getOtIdAtDbColInfoList otDst of
    [x] -> x
    _ -> error $ errMsgHead ++ errMsgDescription
      where
        errMsgHead        = "at_ref_std...: " ++ refColForErrMsg ++
                            " (destination ObjectType = " ++ otCrossRefKey otDst  ++ ")"
        errMsgDescription = ": ID AttributeType of destination ObjectType has more than one database columns"
  where
    getOtIdAtDbColInfoList = NonEmpty.toList . DdlAtAnnotation.atDdlInfo . otIdAttributeType

getSingletonDdlColForRefTarget :: NonEmpty.NonEmpty (DdlColumnInfo dbTable) -> DdlColumnInfo dbTable
getSingletonDdlColForRefTarget =
  getSingleton "at_ref: Invalid Object Model: Invalid num db-cols in ref-target"

ddlColumnInfoForReference :: (Database.DATABASE_TABLE otConf
                             ,SQL_IDENTIFIER dbtSrc
                             )
                          => Bool
                          -> dbtSrc
                          -> ObjectType otConf DdlAtAnnotation.Configuration dbtDst otNativeDst typeForExisting typeForCreate
                          -> DdlColumnInfo dbtDst
                          -> DdlColumnInfo dbtSrc
ddlColumnInfoForReference refIsOptional sourceField otDst@(ObjectType {}) dciDst =
  DdlColumnInfo
  {
    columnIdent            = sourceField
  , hdbcColDesc            = colDesc
  , extraWhenNotForeignKey = extraWhenNotForeignKey dciDst
  , columnExtra            = columnExtra dciDst
  , foreignKey             = Just foreignKeyTarget
  }
  where
    colDesc     = if refIsOptional
                  then colDescDst { colNullable = Just True }
                  else colDescDst
    colDescDst = hdbcColDesc dciDst
    foreignKeyTarget = ForeignKeyTarget
                       {
                         fkTargetTable  = tableName . Database.otDatabaseTable $ otDst
                       , fkTargetColumn = sqlIdentifier . columnIdent $ dciDst
                       }


-------------------------------------------------------------------------------
-- - Bool -
-------------------------------------------------------------------------------


-- | Helper method for constructing 'AttributeType's that have identical types
-- for type-for-existing and type-for-create.
at_EC :: (SQL_IDENTIFIER dbTable,Typeable a,Show a,Read a)
      => (dbTable -> AtDbInfo.AttributeTypeDatabaseInfo_same dbTable a)
      -> (UiIo.AttributeName -> AttributeTypeOutputerForCreate a a)
      -> UserInteractionInputerForAttribute a
      -> PresentationOutputer a
      -> WildeStyle -- ^ Style for presentation
      -> Maybe (UserInteraction.AttributeTypeCreateOption a)
      -> Title      -- ^ UI title
      -> dbTable    -- ^ DB field
      -> PlainAttributeType_ddl dbTable a
at_EC mkAtDbInfo_ddl uiOutputerC uiInputer presOutputer styleForPres createOption title field =
   AttributeType
   {
     atCrossRefKey              = attributeName
   , atPresentationO            = presOutputer
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = presO
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_showRead_nonEmpty
     , UiIoAndDbIo.uiIoForAttributeName     = uiIoE
     , UiIoAndDbIo.uiCreateOption           = createOption
     , UiIoAndDbIo.mediaIoForCreate         = mediaIoC
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbInfo      = mkAtDbInfo_ddl field
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    mediaIoC      = AttributeTypeMediaIoForCreate
                    {
                      aticDatabaseOutputer = AtDbInfo.atdbioCreateOutputer atDbInfo
                    , aticUiIo             = uiIoC
                    }
    uiIoE         = UiIo.UserInteractionIo
                    {
                      UiIo.uiOutputer = atUiOutputerEfromC uiOutputerC
                    , UiIo.uiInputer  = uiInputer
                     }
    uiIoC         = UiIo.UserInteractionIo
                    {
                      UiIo.uiOutputer = uiOutputerC
                    , UiIo.uiInputer  = uiInputer
                    }
    presO         = AttributeTypePresentation
                    {
                      atpoOutput = presOutputer
                    , atpoTitle  = wildeStyling styleForPres title
                    }

-- | Helper method for constructing 'AttributeType's for Bool:s.
--
-- The first arguments specifies representation for User Interaction and
-- Presentation medias.
at_Bool :: SQL_IDENTIFIER dbTable
        => (UiIo.AttributeName -> UiO.WidgetConstructorGetter (AttributeWidgetDefaultValueForCreate Bool Bool))
        -> UserInteractionInputerForAttribute Bool
        -> PresentationOutputer Bool
        -> WildeStyle -- ^ Style for presentation
        -> Maybe (UserInteraction.AttributeTypeCreateOption Bool)
        -> Title   -- ^ UI title
        -> dbTable -- ^ DB field
        -> PlainAttributeType_ddl dbTable Bool
at_Bool = at_EC AtDbInfo.bool

-- | Helper method for constructing 'AttributeType's for optional Bool:s.
--
-- The first arguments specifies representation for User Interaction and
-- Presentation medias.
at_Bool_optional :: SQL_IDENTIFIER dbTable
                    => (UiIo.AttributeName -> UiO.WidgetConstructorGetter (AttributeWidgetDefaultValueForCreate (Maybe Bool) (Maybe Bool)))
                    -> UserInteractionInputerForAttribute (Maybe Bool)
                    -> PresentationOutputer (Maybe Bool)
                    -> WildeStyle -- ^ Style for presentation
                    -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe Bool))
                    -> Title   -- ^ UI title
                    -> dbTable -- ^ DB field
                    -> PlainAttributeType_optional_ddl dbTable Bool
at_Bool_optional = at_EC AtDbInfo.bool_optional

-- | The UI default representation is equal to the Generic String Representation, which
-- is implemented by show.
at_Bool_checkBox :: SQL_IDENTIFIER dbTable
                 => Maybe (UserInteraction.AttributeTypeCreateOption Bool)
                 -> Title   -- ^ UI title
                 -> dbTable -- ^ DB field
                 -> PlainAttributeType_ddl dbTable Bool
at_Bool_checkBox = at_Bool uiOutputerC uiInputer presOutputer neutral
  where
    uiOutputerC :: UiIo.AttributeName -> UiO.WidgetConstructorGetter (AttributeWidgetDefaultValueForCreate Bool Bool)
    uiOutputerC = nonMonadicSimpleUiOutputer parseBoolDefault simpeUiOutputerC

    simpeUiOutputerC :: UiIo.ElementKey -> Maybe Bool -> UiIo.AnyWIDGET
    simpeUiOutputerC ek mbDefault = Widgets.newWidgetWithKeyAny ek $
                                    Widgets.CheckBoxInfo
                                    {
                                      Widgets.checkBoxValue   = "_"
                                    , Widgets.checkBoxDefault = maybe False id mbDefault
                                    }

    uiInputer :: UiIo.AttributeName
              -> UiI.UserInteractionInputer (ElementInputResult Bool)
    uiInputer = UIIs.mkUiInputer_c ESU.justAnythingIsTrue_parser

    presOutputer bool = AnySVALUE $ QuotedStringValue s
      where
        s = htmlBoolChar bool

    parseBoolDefault :: UiIo.GenericWidgetDefaultValue -> Maybe Bool
    parseBoolDefault s = either (const Nothing) Just eitherErrOrBool
      where
        eitherErrOrBool = gsr_parser_read_nonEmpty s

at_Bool_optional_dropDown :: SQL_IDENTIFIER dbTable
                 => Maybe (UserInteraction.AttributeTypeCreateOption (Maybe Bool))
                 -> Title   -- ^ UI title
                 -> dbTable -- ^ DB field
                 -> PlainAttributeType_optional_ddl dbTable Bool
at_Bool_optional_dropDown = at_Bool_optional uiOutputerC uiInputer presOutputer neutral
  where
    uiOutputerC :: UiIo.AttributeName -> UiO.WidgetConstructorGetter (AttributeWidgetDefaultValueForCreate (Maybe Bool) (Maybe Bool))
    uiOutputerC = nonMonadicSimpleUiOutputer parseBoolDefault simpeUiOutputerC

    simpeUiOutputerC :: UiIo.ElementKey -> Maybe (Maybe Bool) -> UiIo.AnyWIDGET
    simpeUiOutputerC ek mbDefault = Widgets.newWidgetWithKeyAny ek $
                                    Widgets.DropDownListInfo
                                    {
                                      Widgets.dropDownOptions = dropDownOptions
                                    , Widgets.dropDownDefault = fmap gsrOutputer mbDefault
                                    }
    dropDownOptions =
      [(gsrOutputer Nothing     ,mkQuotedDropDownPresValue "")
      ,(gsrOutputer (Just False),mkDropDownPresValueB False)
      ,(gsrOutputer (Just True) ,mkDropDownPresValueB True)
      ]

    uiInputer :: UserInteractionInputerForAttribute (Maybe Bool)
    uiInputer = UIIs.mkUiInputer_c $ ESU.gsr_parser gsrInputer

    GenericStringRepIo gsrInputer gsrOutputer = gsrIo_showRead_nonEmpty :: GenericStringRepIo (Maybe Bool)

    presOutputer mbBool = AnySVALUE $ QuotedStringValue s
      where
        s = maybe "" htmlBoolChar mbBool

    parseBoolDefault :: UiIo.GenericWidgetDefaultValue -> Maybe (Maybe Bool)
    parseBoolDefault s = either (const Nothing) Just eitherErrOrBool
      where
        eitherErrOrBool =  gsr_parser_read_nonEmpty s

    mkQuotedDropDownPresValue :: String -> AnyVALUE
    mkQuotedDropDownPresValue = AnyVALUE . QuotedStringValue

    mkDropDownPresValueB :: Bool -> AnyVALUE
    mkDropDownPresValueB = AnyVALUE . QuotedStringValue . htmlBoolChar


-------------------------------------------------------------------------------
-- - Bool -
-------------------------------------------------------------------------------


htmlBoolChar :: Bool -> String
htmlBoolChar False = "&#9744;"
htmlBoolChar True  = "&#9745;" -- (box with checkmark) "&#9746;" (box with cross) "&#x2713;"

unquotedDropDownValue :: String -> AnyVALUE
unquotedDropDownValue = AnyVALUE . UnquotedStringValue

at_Bool_Word32 :: SQL_IDENTIFIER dbTable
                  => Int
                  -> dbTable
                  -> Maybe (UserInteraction.AttributeTypeCreateOption Word32)
                  -> Title
                  -> PlainAttributeType_ddl dbTable Word32
at_Bool_Word32 = at_Word32

at_Bool_Word32_optional :: SQL_IDENTIFIER dbTable
                           => Int
                           -> dbTable
                           -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe Word32))
                           -> Title
                           -> PlainAttributeType_optional_ddl dbTable Word32
at_Bool_Word32_optional = at_Word32_optional


-------------------------------------------------------------------------------
-- - Word32 -
-------------------------------------------------------------------------------


at_Word32 :: SQL_IDENTIFIER dbTable
          => Int
          -> dbTable
          -> Maybe (UserInteraction.AttributeTypeCreateOption Word32)
          -> Title
          -> PlainAttributeType_ddl dbTable Word32
at_Word32 inputWidth field createOption title =
   AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Integral
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Integral title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_convertibleFromInteger_nonEmpty
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Word32 inputWidth)
     , UiIoAndDbIo.uiCreateOption           = createOption
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Word32 inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.word32 field

at_Word32_optional :: SQL_IDENTIFIER dbTable
                      => Int
                      -> dbTable
                      -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe Word32))
                      -> Title
                      -> PlainAttributeType_optional_ddl dbTable Word32
at_Word32_optional inputWidth field createOption title =
   AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Integral_optional
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Integral_optional title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_optional_ValueMissing_is_Nothing atGsrIo_convertibleFromInteger_nonEmpty
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Word32_optional inputWidth)
     , UiIoAndDbIo.uiCreateOption           = createOption
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Word32_optional inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.word32_optional field


-------------------------------------------------------------------------------
-- - Double -
-------------------------------------------------------------------------------


at_Double_withExprUiInput :: SQL_IDENTIFIER dbTable
                          => Int
                          -> dbTable
                          -> Maybe (UserInteraction.AttributeTypeCreateOption Double)
                          -> Title
                          -> PlainAttributeType_ddl dbTable Double
at_Double_withExprUiInput inputWidth field createOption title =
   AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Fractional
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Fractional title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_showRead_nonEmpty
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Double inputWidth)
     , UiIoAndDbIo.uiCreateOption           = createOption
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Double inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = AtDbInfo.mkAtDbConfigForE atDbInfo
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.double field

at_Double_optional_withExprUiInput :: SQL_IDENTIFIER dbTable
                                      => Int
                                      -> dbTable
                                      -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe Double))
                                      -> Title
                                      -> PlainAttributeType_optional_ddl dbTable Double
at_Double_optional_withExprUiInput inputWidth field createOption title =
   AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Fractional_optional
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Fractional_optional title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_showRead_nonEmpty
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Double_optional inputWidth)
     , UiIoAndDbIo.uiCreateOption           = createOption
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Double_optional inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.double_optional field


-------------------------------------------------------------------------------
-- - Date -
-------------------------------------------------------------------------------


at_Date :: SQL_IDENTIFIER dbTable
           => Int
           -> dbTable
           -> Maybe (UserInteraction.AttributeTypeCreateOption Day)
           -> Title
           -> PlainAttributeType_ddl dbTable Day
at_Date inputWidth field defaultForCreate title =
   AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Date
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Date title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_showRead_nonEmpty
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Date inputWidth)
     , UiIoAndDbIo.uiCreateOption           = defaultForCreate
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Date inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.day field

at_Date_optional :: SQL_IDENTIFIER dbTable
                    => Int
                    -> dbTable
                    -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe Day))
                    -> Title
                    -> PlainAttributeType_optional_ddl dbTable Day
at_Date_optional inputWidth field defaultForCreate title =
  AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Date_optional
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Date_optional title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_showRead_nonEmpty
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Date_optional inputWidth)
     , UiIoAndDbIo.uiCreateOption           = defaultForCreate
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Date_optional inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.day_optional field

at_Date_withConvenienteUiInput :: SQL_IDENTIFIER dbTable
                               => Int
                               -> dbTable
                               -> Maybe (UserInteraction.AttributeTypeCreateOption Day)
                               -> Title
                               -> PlainAttributeType_ddl dbTable Day
at_Date_withConvenienteUiInput inputWidth field defaultForCreate title =
  AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Date
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Date title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_showRead_nonEmpty
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Date_withConvenienteUiInput inputWidth)
     , UiIoAndDbIo.uiCreateOption           = defaultForCreate
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Date_withConvenienteUiInput inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.day field

at_Date_optional_withConvenienteUiInput :: SQL_IDENTIFIER dbTable
                    => Int
                    -> dbTable
                    -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe Day))
                    -> Title
                    -> PlainAttributeType_optional_ddl dbTable Day
at_Date_optional_withConvenienteUiInput inputWidth field defaultForCreate title =
  AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Date_optional
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Date_optional title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_showRead_nonEmpty
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Date_optional_withConvenienteUiInput inputWidth)
     , UiIoAndDbIo.uiCreateOption           = defaultForCreate
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Date_optional_withConvenienteUiInput inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.day_optional field


-------------------------------------------------------------------------------
-- - String -
-------------------------------------------------------------------------------


at_String_forDefaultDbIo :: SQL_IDENTIFIER dbTable
                         => Int
                         -> Int
                         -> dbTable
                         -> Maybe (UserInteraction.AttributeTypeCreateOption String)
                         -> Title
                         -> PlainAttributeType_ddl dbTable String
at_String_forDefaultDbIo = at_String dbIo_string_default

at_String :: SQL_IDENTIFIER dbTable
          => DatabaseIo String
          -> Int
          -> Int
          -> dbTable
          -> Maybe (UserInteraction.AttributeTypeCreateOption String)
          -> Title
          -> PlainAttributeType_ddl dbTable String
at_String dbIo maxSize inputWidth field defaultForCreate title =
  AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_String
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_String title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_string
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_String inputWidth)
     , UiIoAndDbIo.uiCreateOption           = defaultForCreate
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_String inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.string_forDbIo dbIo maxSize field

at_String_optional_forDefaultDbIo :: SQL_IDENTIFIER dbTable
                                  => Int
                                  -> Int
                                  -> dbTable
                                  -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe String))
                                  -> Title
                                  -> PlainAttributeType_optional_ddl dbTable String
at_String_optional_forDefaultDbIo = at_String_optional dbIo_string_optional_default

at_String_optional :: SQL_IDENTIFIER dbTable
                   => DatabaseIo (Maybe String)
                   -> Int
                   -> Int
                   -> dbTable
                   -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe String))
                   -> Title
                   -> PlainAttributeType_optional_ddl dbTable String
at_String_optional dbIo maxSize inputWidth field defaultForCreate title =
  AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_String_optional
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_String_optional title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_optional_ValueMissing_is_Nothing atGsrIo_string
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_String_optional inputWidth)
     , UiIoAndDbIo.uiCreateOption           = defaultForCreate
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_String_optional inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.string_optional_forDbIo dbIo maxSize field


-------------------------------------------------------------------------------
-- - Href -
-------------------------------------------------------------------------------


at_Href :: SQL_IDENTIFIER dbTable
        => DatabaseIo String
        -> Int
        -> Int
        -> dbTable
        -> Maybe (UserInteraction.AttributeTypeCreateOption String)
        -> Title
        -> PlainAttributeType_ddl dbTable String
at_Href dbIo maxSize inputWidth field defaultForCreate title =
  AttributeType
  {
    atCrossRefKey              = sqlIdentifier field
  , atPresentationO            = presO_Href
  , atConfiguration =
    UiIoAndDbIo.Configuration
    {
      UiIoAndDbIo.presentationO            = pres_Href title
    , UiIoAndDbIo.genericStringRepIo       = atGsrIo_string
    , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_String inputWidth)
    , UiIoAndDbIo.uiCreateOption           = defaultForCreate
    , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_String inputWidth) atDbInfo
    , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
    , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
    , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
    }
  }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.string_forDbIo dbIo maxSize field

at_Href_optional :: SQL_IDENTIFIER dbTable
                 => DatabaseIo (Maybe String)
                 -> Int
                 -> Int
                 -> dbTable
                 -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe String))
                 -> Title
                 -> PlainAttributeType_optional_ddl dbTable String
at_Href_optional dbIo maxSize inputWidth field defaultForCreate title =
  AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Href_optional
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Href_optional title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_optional_ValueMissing_is_Nothing atGsrIo_string
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_String_optional inputWidth)
     , UiIoAndDbIo.uiCreateOption           = defaultForCreate
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_String_optional inputWidth) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    atDbConfigE   = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.string_optional_forDbIo dbIo maxSize field


-------------------------------------------------------------------------------
-- - Enum -
-------------------------------------------------------------------------------


at_EnumAsDropDown_Word32 :: SQL_IDENTIFIER dbTable
                         => [(Word32,AnyVALUE)]
                         -> dbTable
                         -> Maybe (UserInteraction.AttributeTypeCreateOption Word32)
                         -> WildeTitle
                         -> PlainAttributeType_ddl dbTable Word32
at_EnumAsDropDown_Word32 = at_EnumAsDropDown_mandatory AtDbInfo.word32 (uiIo_Word32 inputWidth)
  where
    inputWidth = 5

at_EnumAsDropDown_Word32_optional :: SQL_IDENTIFIER dbTable
                                  => [(Word32,AnyVALUE)]
                                  -> dbTable
                                  -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe Word32))
                                  -> WildeTitle
                                  -> PlainAttributeType_optional_ddl dbTable Word32
at_EnumAsDropDown_Word32_optional values =
  at_EnumAsDropDown_optional
  AtDbInfo.word32_optional
  (uiIo_asDropDown_optional values)
  values

at_EnumAsDropDown_mandatory :: (Eq a,Typeable a,Show a,Read a,SQL_IDENTIFIER dbTable)
                            => (dbTable -> AtDbInfo.AttributeTypeDatabaseInfo_same dbTable a)
                            -> AttributeTypeUserInteractionIo a a
                            -> [(a,AnyVALUE)]
                            -> dbTable
                            -> Maybe (UserInteraction.AttributeTypeCreateOption a)
                            -> WildeTitle
                            -> PlainAttributeType_ddl dbTable a
at_EnumAsDropDown_mandatory mkAtDbInfoForColumn atUiIo values
  field createOption presSpec
  =
    AttributeType
    {
      atCrossRefKey              = sqlIdentifier field
    , atPresentationO            = presO
    , atConfiguration =
      UiIoAndDbIo.Configuration
      {
        UiIoAndDbIo.presentationO            = atPres
      , UiIoAndDbIo.genericStringRepIo       = atGsrIo_showRead_nonEmpty
      , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo atUiIo
      , UiIoAndDbIo.uiCreateOption           = createOption
      , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate atUiIo atDbInfo
      , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
      , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
      , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
      }
    }
  where
    presO             = withNeutralStyleAny . lookupPresVal
    atDbInfo          = mkAtDbInfoForColumn field
    atDbConfigE       = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName     = sqlIdentifier field
    atPres =
      AttributeTypePresentation
      {
        atpoOutput = withNeutralStyleAny . lookupPresVal,
        atpoTitle  = presSpec
      }
    lookupPresVal a = maybe (unquotedDropDownValue ("INVALID ENUM: " ++ show a)) id (lookup a values)

at_EnumAsDropDown_optional :: (Eq a,Typeable a,Show a,Read a,SQL_IDENTIFIER dbTable)
                           => (dbTable -> AtDbInfo.AttributeTypeDatabaseInfo_same dbTable (Maybe a))
                           -> AttributeTypeUserInteractionIo (Maybe a) (Maybe a)
                           -> [(a,AnyVALUE)]
                           -> dbTable
                           -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe a))
                           -> WildeTitle
                           -> PlainAttributeType_ddl dbTable (Maybe a)
at_EnumAsDropDown_optional mkAtDbInfoForColumn atUiIo values
  field createOption presSpec
  =
    AttributeType
    {
      atCrossRefKey              = sqlIdentifier field
    , atPresentationO            = presO
    , atConfiguration =
      UiIoAndDbIo.Configuration
      {
        UiIoAndDbIo.presentationO            = atPres
      , UiIoAndDbIo.genericStringRepIo       = atGsrIo_showRead_nonEmpty
      , UiIoAndDbIo.uiIoForAttributeName     = attributeTypeUserInteractionIoForExisting
      , UiIoAndDbIo.uiCreateOption           = createOption
      , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate atUiIo atDbInfo
      , UiIoAndDbIo.databaseConfForExisting  = atDbConfigE
      , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
      , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
      }
    }
  where
    atDbInfo          = mkAtDbInfoForColumn field
    atDbConfigE       = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName     = sqlIdentifier field
    presO             = mkOptional (withNeutralStyleAny . lookupPresVal)
    atPres =
      AttributeTypePresentation
      {
        atpoOutput = presO
      , atpoTitle  = presSpec
      }
    lookupPresVal a   = maybe
                        (unquotedDropDownValue ("INVALID ENUM: " ++ show a))
                        id (lookup a values)
    attributeTypeUserInteractionIoForExisting = atuiioExistingIo atUiIo


-------------------------------------------------------------------------------
-- - Text -
-------------------------------------------------------------------------------


at_Text :: SQL_IDENTIFIER dbTable
        => DatabaseIo String
        -> (Int,Int)
        -> dbTable
        -> Maybe (UserInteraction.AttributeTypeCreateOption String)
        -> Title
        -> PlainAttributeType_ddl dbTable String
at_Text dbIo size field createOption title =
  AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Text
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Text title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_string
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Text size)
     , UiIoAndDbIo.uiCreateOption           = createOption
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Text size) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = dbIoE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    dbIoE         = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.longString dbIo field

at_Text_optional :: SQL_IDENTIFIER dbTable
                 => DatabaseIo String
                 -> (Int,Int)
                 -> dbTable
                 -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe String))
                 -> Title
                 -> PlainAttributeType_optional_ddl dbTable String
at_Text_optional dbIo size field createOption title =
  AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_Text_optional
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_Text_optional title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_optional_ValueMissing_is_Nothing atGsrIo_string
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Text_optional size)
     , UiIoAndDbIo.uiCreateOption           = createOption
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Text_optional size) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = dbIoE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    dbIoE         = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.longString_optional dbIo field


at_Text_html_optional :: SQL_IDENTIFIER dbTable
                      => DatabaseIo String
                      -> (Int,Int)
                      -> dbTable
                      -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe String))
                      -> Title
                      -> PlainAttributeType_optional_ddl dbTable String
at_Text_html_optional dbIo size field createOption title =
  AttributeType
   {
     atCrossRefKey              = sqlIdentifier field
   , atPresentationO            = presO_TextHtml_optional
   , atConfiguration =
     UiIoAndDbIo.Configuration
     {
       UiIoAndDbIo.presentationO            = pres_TextHtml_optional title
     , UiIoAndDbIo.genericStringRepIo       = atGsrIo_optional_ValueMissing_is_Nothing atGsrIo_string
     , UiIoAndDbIo.uiIoForAttributeName     = atuiioExistingIo (uiIo_Text_optional size)
     , UiIoAndDbIo.uiCreateOption           = createOption
     , UiIoAndDbIo.mediaIoForCreate         = atInfoForCreate (uiIo_Text_optional size) atDbInfo
     , UiIoAndDbIo.databaseConfForExisting  = dbIoE
     , UiIoAndDbIo.dbPresentationInfoGetter = AttributeWithPresentationInfoDbInputerInfo Nothing
     , UiIoAndDbIo.annotation               = ddlAnnotation atDbInfo
     }
   }
  where
    dbIoE         = AtDbInfo.mkAtDbConfigForE atDbInfo
    attributeName = sqlIdentifier field
    atDbInfo      = AtDbInfo.longString_optional dbIo field


-------------------------------------------------------------------------------
-- - GenericWidgetDefaultValue -
-------------------------------------------------------------------------------


at_GenericWidgetDefaultValue :: SQL_IDENTIFIER dbTable
                             => Int
                             -> Int
                             -> dbTable
                             -> Maybe (UserInteraction.AttributeTypeCreateOption UiIo.GenericWidgetDefaultValue)
                             -> Title
                             -> PlainAttributeType_ddl dbTable UiIo.GenericWidgetDefaultValue
at_GenericWidgetDefaultValue = at_String_forDefaultDbIo

at_GenericWidgetDefaultValue_optional :: SQL_IDENTIFIER dbTable
                                      => Int
                                      -> Int
                                      -> dbTable
                                      -> Maybe (UserInteraction.AttributeTypeCreateOption (Maybe UiIo.GenericWidgetDefaultValue))
                                      -> Title
                                      -> PlainAttributeType_optional_ddl dbTable UiIo.GenericWidgetDefaultValue
at_GenericWidgetDefaultValue_optional = at_String_optional_forDefaultDbIo


-------------------------------------------------------------------------------
-- - ddl -
-------------------------------------------------------------------------------


ddlAnnotation :: AtDbInfo.AttributeTypeDatabaseInfo dbTable e c
              -> DdlAtAnnotation.DdlAtAnnotation dbTable e c
ddlAnnotation = DdlAtAnnotation.mkDdlAtAnnotation .
                AtDbInfo.atdbiofeStructure .
                AtDbInfo.atdbioInfoForExisting


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------


getSingleton :: String -> NonEmpty.NonEmpty a -> a
getSingleton errMsgHead l =
  case NonEmpty.length l of
    1 -> NonEmpty.head l
    n -> error $ errMsgHead ++ ": Num elements should be exactly 1. Now: " ++ show n
