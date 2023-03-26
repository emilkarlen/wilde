{-# LANGUAGE FlexibleContexts #-}

module ObjectModel where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Database.Sql

import           Wilde.WildeUi.UiPrimitives (WildeTitle)

import           Wilde.Media.Database

import           Wilde.ObjectModel.ObjectModelUtils
import qualified Wilde.ObjectModel.UserInteraction as UserInteraction

import           Wilde.Driver.Database.MySQL.ApplicationObjectModelTools as ToolsMySql

import           Wilde.ApplicationConstruction.ObjectModel.ObjectType
import           Wilde.ApplicationConstruction.StandardServices.Tools
import           Wilde.ApplicationConstruction.StandardServices as StandardServices
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ReferringObjectsComponent as SOC
import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.AttributeTypesListFooter as AtFooter
import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDbConfig
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation as ObjectTypeWithAtDdlInformation

import qualified ObjectType.CharacterEncodingExperiment as CharacterEncodingExperiment

import qualified AttributeType as MyAt


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Object Model -
-------------------------------------------------------------------------------


objectModel :: [ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup]
objectModel = [aotsReference
              ,aotsAutoincPK
              ,CharacterEncodingExperiment.aots
              ,aotsSimpleDatatypes
              ]


-------------------------------------------------------------------------------
-- - Simple Datatypes -
-------------------------------------------------------------------------------


{-
Återstår:

  nInt	INT NOT NULL, (borde ha en variant med uttrycks-eval (finns en evaluator))
  tTime	TIME,
  dtDatetime	DATETIME,

-}

data SdTable = SdId
             | SdString
             | SdBoolean
             | SdDouble
             | SdDate
             | SdText
             | SdUrl
             | SdHtmlText

instance SQL_IDENTIFIER SdTable where
  sqlIdentifier SdId       = "idSdId"
  sqlIdentifier SdString   = "sString"
  sqlIdentifier SdBoolean  = "bBoolean"
  sqlIdentifier SdDouble   = "fReal"
  sqlIdentifier SdDate     = "dDate"
  sqlIdentifier SdText     = "txtText"
  sqlIdentifier SdUrl      = "sUrl"
  sqlIdentifier SdHtmlText = "txtHtmlText"

type SimpleDatatypes = (Word32,Maybe String,String)

titleSimpleDatatypes :: WildeTitle
titleSimpleDatatypes = wildeStyling (WildeStyle ["simple_datatypes"]) "Simple Datatypes"

aotsSimpleDatatypes :: ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup
aotsSimpleDatatypes = ObjectTypeWithAtDdlInformation.AnyO otsSimpleDatatypes

otsSimpleDatatypes :: StandardServices.ObjectTypeSetup OtDbConfig.Configuration DdlAtAnnotation.Configuration SdTable SimpleDatatypes Word32 Word32
otsSimpleDatatypes = (objectTypeSetup otSimpleDatatypes titleSimpleDatatypes)
                     `withDependentComponents`
                     [SOC.mandatory
                      otsReference
                      ref_SimpleDatatypes
                     ]
                     `withModifiedObjectListDisplaySetup`
                     (setFooterRowsConstructor $
                      AtFooter.attributeTypesFooterCellsGetter
                      otSimpleDatatypes
                      [AtFooter.mkAtFooterSpec
                       sdAtDouble
                       [AtFooter.toOptional AtFooter.sumCell_show]
                      ]
                     )

otSimpleDatatypes :: ObjectType OtDbConfig.Configuration DdlAtAnnotation.Configuration SdTable SimpleDatatypes Word32 Word32
otSimpleDatatypes =
  ObjectType
  {
    otCrossRefKey             = "simple_datatypes"
  , otIdAttributeType         = sdAtId,
    otNonIdAttributeTypes     = [Any sdAtName
                                ,Any sdAtBoolean
                                ,Any sdAtDouble
                                ,Any sdAtDate
                                ,Any sdAtText
                                ,Any sdAtUrl
                                ,Any sdAtHtmlText
                                ]
  , otToNative                = sdToNative
  , otConfiguration =
    OtDbConfig.Configuration
    {
      OtDbConfig.databaseTable               = sdDatabaseTable
    , OtDbConfig.getIdOfInsertedIntoDatabase = getIdOfInsertedWhenEqualToCreate
    }
  }

rpsSimpleDatatypes :: ReferencePresentationSpec otConf DdlAtAnnotation.Configuration SdTable otNative idAtExisting idAtCreate
rpsSimpleDatatypes = refPresSpec_default $
                     PresentationAttributeTypeInfo sdAtName (maybe "<anonymous>" id)

sdDatabaseTable :: DatabaseTable
sdDatabaseTable = DatabaseTable "simple_datatypes"

sdAtId :: PlainAttributeType_ddl SdTable Word32
sdAtId = at_Word32 5 SdId Nothing "ID"

sdAtName :: PlainAttributeType_optional_ddl SdTable String
sdAtName = MyAt.at_Name_optional "String" SdString

sdAtBoolean :: PlainAttributeType_optional_ddl SdTable Bool
sdAtBoolean = at_Bool_optional_dropDown Nothing "Boolean (opt)" SdBoolean

sdAtDouble :: PlainAttributeType_optional_ddl SdTable Double
sdAtDouble = at_Double_optional_withExprUiInput
             20
             SdDouble
             (Just (UserInteraction.AtuicoDefault (Left "1 + 2")))
             "Double"

sdAtDate :: PlainAttributeType_optional_ddl SdTable Day
sdAtDate = at_Date_optional_withConvenienteUiInput
           15
           SdDate
           (Just (UserInteraction.AtuicoDefault (Left "0")))
           "Date"

sdAtText :: PlainAttributeType_optional_ddl SdTable String
sdAtText = at_Text_optional
           MyAt.dbIo_string
           (40,4)
           SdText
           Nothing
           "Text"

sdAtUrl :: PlainAttributeType_optional_ddl SdTable String
sdAtUrl = at_Href_optional
          MyAt.dbIo_string_optional
          200
          30
          SdUrl
          Nothing
          "Url"

sdAtHtmlText :: PlainAttributeType_optional_ddl SdTable String
sdAtHtmlText = at_Text_html_optional
               MyAt.dbIo_string
               (40,4)
               SdHtmlText
               Nothing
               "HtmlText"

sdToNative :: ObjectToNativeFunction SdTable SimpleDatatypes Word32 Word32
sdToNative = ObjectToNativeFunction f
  where
    f o =
      let
        word32 = attrValue $ oIdAttribute o
      in
       case oNonIdAttributes o of
         [Any aString@(Attribute {})] ->
           do
             mbString <- doOtnUnhide $ attrValue aString
             pure (word32,mbString,"Dummy")
         attrs -> numAttributesError2 attrs 1


-------------------------------------------------------------------------------
-- - Autoinc PK -
-------------------------------------------------------------------------------


data AutoincTable = AutoincId | AutoincString

instance SQL_IDENTIFIER AutoincTable where
  sqlIdentifier AutoincId     = "id"
  sqlIdentifier AutoincString = "string"

type AutoincPK = (PrimaryKeyType,Maybe String)

titleAutoincPK :: WildeTitle
titleAutoincPK = wildeStyling
                 (WildeStyle ["auto_inc"])
                 "Autoinc PK"

aotsAutoincPK :: ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup
aotsAutoincPK = ObjectTypeWithAtDdlInformation.AnyO $ otsAutoincPK

rpsAutoincPK :: ReferencePresentationSpec OtDbConfig.Configuration DdlAtAnnotation.Configuration AutoincTable otNative idAtExisting idAtCreate
rpsAutoincPK = refPresSpec_default $
               PresentationAttributeTypeInfo autoincAtName (maybe "<anonymous>" id)


otsAutoincPK :: StdAutoPkObjectTypeSetup_ddl AutoincTable AutoincPK
otsAutoincPK = objectTypeSetup otAutoincPK titleAutoincPK

otAutoincPK :: StdAutoPkObjectType_ddl AutoincTable AutoincPK
otAutoincPK = ToolsMySql.ot_PrimaryKey_dbAutogen_MySql
              autoincDatabaseTable
              autoincToNative
              (at_PrimaryKey_dbAutogen 10 AutoincId)
              nonIdAttributeTypes
  where
    nonIdAttributeTypes = [Any autoincAtName]

autoincDatabaseTable :: DatabaseTable
autoincDatabaseTable = DatabaseTable "auto_inc"

autoincAtName :: PlainAttributeType_optional_ddl AutoincTable String
autoincAtName = MyAt.at_Name_optional
                "string"
                AutoincString

autoincToNative :: ObjectToNativeFunction AutoincTable AutoincPK PrimaryKeyType (Maybe PrimaryKeyType)
autoincToNative = ObjectToNativeFunction f
  where
    f o =
      let
        pkValue = attrValue $ oIdAttribute o
      in
       case oNonIdAttributes o of
         [Any aString@(Attribute {})] ->
           do
             mbString <- doOtnUnhide $ attrValue aString
             pure (pkValue,mbString)
         attrs -> numAttributesError2 attrs 1


-------------------------------------------------------------------------------
-- - Reference -
-------------------------------------------------------------------------------


data RefTable = RefId
              | RefSd
              | RefAutoInc
              | RefAutoIncOpt

instance SQL_IDENTIFIER RefTable where
  sqlIdentifier RefId         = "idRefId"
  sqlIdentifier RefSd         = "sds_idSdId"
  sqlIdentifier RefAutoInc    = "idAutoIncId"
  sqlIdentifier RefAutoIncOpt = "idAutoIncId_opt"

type ReferenceNative = (Word32,Int32,Int32)

titleReference :: WildeTitle
titleReference = wildeStyling
                 (WildeStyle ["reference"])
                 "Reference"

aotsReference :: ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup
aotsReference = ObjectTypeWithAtDdlInformation.AnyO $ otsReference

otsReference :: StandardServices.ObjectTypeSetup OtDbConfig.Configuration DdlAtAnnotation.Configuration RefTable ReferenceNative Word32 Word32
otsReference = objectTypeSetup otReference titleReference

otReference :: StdObjectType_ddl RefTable ReferenceNative
otReference =
  ObjectType
  {
    otCrossRefKey             = "reference"
  , otIdAttributeType         = refAtId
  , otNonIdAttributeTypes     = [Any ref_SimpleDatatypes
                                ,Any ref_AutoInc_opt
                                ,Any ref_AutoInc]
  , otToNative                = refToNative
  , otConfiguration =
    OtDbConfig.Configuration
    {
      OtDbConfig.databaseTable               = refDatabaseTable
    , OtDbConfig.getIdOfInsertedIntoDatabase = getIdOfInsertedWhenEqualToCreate
    }
  }

refDatabaseTable :: DatabaseTable
refDatabaseTable = DatabaseTable "reference"

refAtId :: PlainAttributeType_ddl RefTable Word32
refAtId = at_PrimaryKey 5 RefId

ref_SimpleDatatypes :: PlainAttributeType_ddl RefTable Word32
ref_SimpleDatatypes = at_ref_std RefSd otsSimpleDatatypes
                      rpsSimpleDatatypes
                      Nothing

ref_AutoInc :: PlainAttributeType_ddl RefTable Word32
ref_AutoInc = at_ref_std RefAutoInc
              otsAutoincPK
              rpsAutoincPK
              Nothing

ref_AutoInc_opt :: PlainAttributeType_optional_ddl RefTable Word32
ref_AutoInc_opt = at_ref_std_optional RefAutoIncOpt
                  otsAutoincPK
                  rpsAutoincPK
                  (Just $ withNeutralWildeStyle "AutoInc (opt)")

refToNative :: ObjectToNativeFunction RefTable ReferenceNative Word32 Word32
refToNative = ObjectToNativeFunction f
  where
    f o = case oNonIdAttributes o of
      [Any aRefSd@(Attribute {}),
       Any aRefAutoincPK@(Attribute {})] ->
        do
          refInt32Sd <- doOtnUnhide $ attrValue aRefSd
          refInt32Ai <- doOtnUnhide $ attrValue aRefAutoincPK
          pure (idInt32,refInt32Sd,refInt32Ai)
      attrs -> numAttributesError2 attrs 1
      where
        idInt32 = attrValue $ oIdAttribute o
