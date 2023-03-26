-------------------------------------------------------------------------------
-- | A component that displays a list of 'Object's that
-- has a reference to another given \"target\" 'Object'.
--
-- Such a component is suitable to display on the page that displays
-- the target object.
-------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ApplicationConstruction.UserInteraction.Output.ReferringObjectsComponent
       (
         mandatory,
         optional,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Database.SqlJoin as Sql

import qualified Wilde.Media.Presentation as Presentation
import           Wilde.Media.WildeValue
import qualified Wilde.Media.WildeStyle as WS
import qualified Wilde.Media.Database.Monad as DbConn


import           Wilde.ObjectModel.ObjectModel

import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup

import qualified Wilde.ObjectModel.GenericStringRep as OmGsr

import qualified Wilde.ObjectModel.Database.Sql.WithPresentationInfo as SqlWithPres
import qualified Wilde.ObjectModel.Database.Execution.SelectWithPresentationInfo as InputPres
import qualified Wilde.ObjectModel.Database.JoinUtils as OmDbJ
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import           Wilde.ObjectModel.UserInteraction

import qualified Wilde.ApplicationConstruction.StandardServices as StandardServices
import           Wilde.ApplicationConstruction.UserInteraction.Output.ObjectDependentComponent
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.StandardFilterExpression as StdFilterExpr
import qualified Wilde.ApplicationConstruction.Presentation.DataAndButtonsComponent as DabComp
import qualified Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectListComponent as OLC
import qualified Wilde.ObjectModel.Presentation.FooterRowsConstructor as FRC


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | A dependent component that displays a list of 'Object's of a type
-- that has a mandatory reference to the super 'ObjectType'.
--
-- NOTE: The type of existing and for-create of the referencing 'AttributeType'
-- are required to be identical because it should work to construct a
-- link-button that constructs a new sub-object with the reference 'AttributeType'
-- fixed to that of the current super 'Object'.
--
-- /FILTERING/
--
-- If the variable _selection.<object-type-cross-ref-identifier>
-- exists in the Custom Environment,
-- it must be an SQL expression on the 'ObjectType' of the component.
--
-- Only 'Object's that satisfy this expression are included in the list.
-------------------------------------------------------------------------------
mandatory :: (Database.DATABASE_TABLE otConf
             ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
             ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
             ,OmGsr.ATTRIBUTE_IO_FOR_EXISTING atConf
             ,OmGsr.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
             )
          => StandardServices.ObjectTypeSetup otConf atConf dbTableSub otNativeSub idAtESub idAtCSub
          -> AttributeType                           atConf dbTableSub             idAtESup idAtESup
          -> ObjectDependentComponent         otConf atConf dbTableSup otNativeSup idAtESup idAtCSup
mandatory
  otSubSetup@(StandardServices.ObjectTypeSetup
              {
                StandardServices.objectType = otSub
              })
  atSuperRef oSuper =
  mkComponent otSub config atSuperRef superPk
  where
    config  = mkShowAllReferringObjsSetup otSubSetup atSuperRef
    superPk = attrValue $ oIdAttribute oSuper

-------------------------------------------------------------------------------
-- | A dependent component that displays a list of 'Object's of a type
-- that has an optional reference to the super 'ObjectType'.
--
-- NOTE: The type of existing and for-create of the referencing 'AttributeType'
-- are required to be identical because: see
-- 'mandatory'.
--
-- See \"FILTERING\" for 'mandatory'.
-------------------------------------------------------------------------------
optional :: (Database.DATABASE_TABLE otConf
            ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
            ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
            ,OmGsr.ATTRIBUTE_IO_FOR_EXISTING atConf
            ,OmGsr.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
            )
         => StandardServices.ObjectTypeSetup otConf atConf dbTableSub otNativeSub idAtESub      idAtCSub
         -> AttributeType                           atConf dbTableSub             (Maybe idAtE) (Maybe idAtE)
         -> ObjectDependentComponent         otConf atConf dbTable    otNative    idAtE         idAtC
optional
  otSubSetup@(StandardServices.ObjectTypeSetup
              {
                StandardServices.objectType = otSub
              })
  atSuperRef oSuper =
  mkComponent otSub config atSuperRef (Just superPk)
  where
    config  = mkShowAllReferringObjsSetup otSubSetup atSuperRef
    superPk = attrValue $ oIdAttribute oSuper

mkComponent
  :: forall otConf atConf ref dbTable otN idAtE idAtCreate.
     (Database.DATABASE_TABLE otConf
     ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
     ,OmGsr.ATTRIBUTE_IO_FOR_EXISTING atConf
     ,OmGsr.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
     )
  => ObjectType            otConf atConf     dbTable otN idAtE idAtCreate
  -> ShowAllReferingConfig otConf atConf ref dbTable otN idAtE idAtCreate
  -> AttributeType                atConf     dbTable ref ref
  -> ref
  -> Presentation.Monad (Maybe AnyCOMPONENT)
mkComponent ot
  (ShowAllReferingConfig
   {
     title = theTitle
   , displaySetup = (OLS.ObjectListDisplaySetup
                     {
                       OLS.displayAts               = theDisplayAts
                     , OLS.orderByInDb              = theOrderByInDb
                     , OLS.getFooterRowsConstructor = theGetFooterRowsConstructor
                     })
   , mkButtonsSetup = theMkButtonsSetup
   }
  )
  atSuperRef
  refVal =
  do
    atListSetup <- Presentation.toPresentationMonad $
                   AttributeTypeListSetup.mkExclude
                   ot
                   atsIncludeBeforeExclusion
                   [Any atSuperRef]
    footerRowsConstructor <- theGetFooterRowsConstructor
    getWhereExpr          <- getWhereExprGetter ot getSubObjWhereExpr
    os                    <- Presentation.toPresentationMonad_wDefaultDbConn $
                             inputObjects getWhereExpr
    dataComponent         <- OLC.objectList
                             (tableStyle os)
                             mbTitle
                             atListSetup
                             (FRC.asFrc2_mb footerRowsConstructor)
                             (OLS.objectButtonsLeft  buttonsSetup)
                             (OLS.objectButtonsRight buttonsSetup)
                             (pure os)
    buttonsBelow          <- sequence $ OLS.objectTypeButtonsBelow buttonsSetup
    pure $ Just $ DabComp.new dataComponent buttonsBelow
  where
    buttonsSetup              = theMkButtonsSetup refVal
    atsIncludeBeforeExclusion = theDisplayAts
    tableStyle                :: [a] -> WildeStyle
    tableStyle os             = tableStyleForObjectTypeSubObjects (null os)
    mbTitle                   :: Maybe WildeTitle
    mbTitle                   = Just $
                                  theTitle
                                  `withAdjustedStyle`
                                   addStyle (WildeStyle [WS.componentClass])
    (getSubObjWhereExpr,getSqlParamsForRef) = OmDbJ.atExprEq atSuperRef

    createOneSubObjButton :: Presentation.Monad AnySVALUE
    createOneSubObjButton = StandardServices.createOneLinkButtonWithFixedAttributeTypes
                            ot fixedValues
      where
        fixedValues = [attributeFixForCreate_fromValue atSuperRef refVal]

    inputObjects :: Sql.JoinMonad dbTable (Maybe (Sql.SqlExpr (Sql.BasedOn dbTable)))
                 -> DbConn.Monad [Object otConf atConf dbTable otN idAtE idAtCreate]
    inputObjects getWhereExpr =
      InputPres.inputForConvertibleParams ot
                                          (SqlWithPres.otDatabaseOrderBy theOrderByInDb)
                                          getWhereExpr
                                          (getSqlParamsForRef refVal)

-- | Constructs a 'StandardServices.ServiceLinkRenderer' for
-- the standard service 'StandardServices.CreateOne'.
createOneLinkRenderer :: StandardServices.StandardServiceLinkRenderer
                      -> StandardServices.ServiceLinkRenderer
createOneLinkRenderer serviceLinkRenderer =
  StandardServices.newLinkRenderer StandardServices.CreateOne serviceLinkRenderer

-------------------------------------------------------------------------------
-- Gets the \"getter\" of the WHERE expression to use when selection the
-- 'Object's to display in the component.
--
-- This function combines the mandatory where-expression that selects only
-- sub-'Object's with a custom expression spcified in the 'CustomEnvironment'.
--
-- The element-key for the custom expression is
-- 'VarNames.varNameSelectExpression' followed by the cross-ref-identifier
-- of the 'ObjectType'.
-------------------------------------------------------------------------------
getWhereExprGetter :: Database.COLUMN_NAMES atConf
                   => ObjectType otConf atConf dbTable otN idAtE idAtCreate
                   -> Sql.JoinMonad dbTable (Sql.SqlExpr (Sql.BasedOn dbTable))
                   -> Presentation.Monad (Sql.JoinMonad dbTable (Maybe (Sql.SqlExpr (Sql.BasedOn dbTable))))
getWhereExprGetter otSubObj@(ObjectType {}) getSubObjWhereExpr =
  do
    getMbExprFromEnv <- StdFilterExpr.lookupExpression_BasedOn otSubObj
    pure $ do
      subObjExpr    <- getSubObjWhereExpr
      mbExprFromEnv <- getMbExprFromEnv
      pure $ Just $ maybe subObjExpr (Sql.binOp Sql.andOp subObjExpr) mbExprFromEnv

tableStyleForObjectTypeSubObjects :: Bool -> WildeStyle
tableStyleForObjectTypeSubObjects listIsEmpty = WildeStyle stdClasses
  where
    stdStyle   = WildeStyle stdClasses
    stdClasses = multiEmpty <> WS.presMultiClasses <>
                 [WS.componentClass
                 ,WS.weObjectClass
                 ,WS.subObjectListClass]
    multiEmpty = [WS.multiEmptyClass | listIsEmpty]

-- | Configuration for displaying a list of sub-objects (refering objects).
data ShowAllReferingConfig otConf atConf atRef dbTable otNative idAtExisting idAtCreate =
  (Database.COLUMNS_AND_IO_FOR_EXISTING atConf)
  => ShowAllReferingConfig
  {
    title          :: WildeTitle
  , displaySetup   :: OLS.ObjectListDisplaySetup otConf atConf dbTable otNative idAtExisting idAtCreate
  , mkButtonsSetup :: atRef -> OLS.ObjectListButtonsSetup otConf atConf dbTable otNative idAtExisting idAtCreate
  }

mkShowAllReferringObjsSetup :: (Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                               ,OmGsr.ATTRIBUTE_IO_FOR_EXISTING atConf
                               ,OmGsr.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
                               )
                            => StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
                            -> AttributeType                           atConf dbTable ref ref
                            -> ShowAllReferingConfig            otConf atConf ref dbTable otNative idAtExisting idAtCreate
mkShowAllReferringObjsSetup
  (StandardServices.ObjectTypeSetup
   {
     StandardServices.titleWithStyle  = tws
   , StandardServices.objectType      = ot
   , StandardServices.objectListSetup = objectListSetup
   })
  atDstRef =
    ShowAllReferingConfig
    {
      title          = tws
    , displaySetup   = displaySetupFromStandardServices
    , mkButtonsSetup = theMkButtonsSetup
    }
  where
    theMkButtonsSetup refVal =
      OLS.ObjectListButtonsSetup
      {
        OLS.objectButtonsLeft      = OLS.objectButtonsLeft  buttonsSetupFromStandardServices
      , OLS.objectButtonsRight     = OLS.objectButtonsRight buttonsSetupFromStandardServices
      , OLS.objectTypeButtonsBelow = [createOneReferingObjButton refVal]
      }
    buttonsSetupFromStandardServices = OLS.buttonsSetup objectListSetup
    displaySetupFromStandardServices = OLS.displaySetup objectListSetup
    createOneReferingObjButton dstPk = StandardServices.createOneLinkButtonWithFixedAttributeTypes
                                       ot fixedValues
      where
        fixedValues = [attributeFixForCreate_fromValue atDstRef dstPk]


-------------------------------------------------------------------------------
-- - AttributeFixedValue -
-------------------------------------------------------------------------------


attributeFixForCreate_fromValue :: OmGsr.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
                                => AttributeType atConf dbTable typeForExisting typeForCreate
                                -> typeForCreate
                                -> AttributeFixedValue atConf dbTable
attributeFixForCreate_fromValue at value =
  AttributeFixedValue (Any at) (outputer value)
  where
    outputer = OmGsr.atOutputerForCreate at
