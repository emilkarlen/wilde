-------------------------------------------------------------------------------
-- | Components for use by an application.
-------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ApplicationConstruction.UserInteraction.Output.SpecialComponents
       (
         objectListTableAccordingToSetup,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Media.WildeValue
import qualified Wilde.Media.Presentation as Presentation

import           Wilde.WildeUi.WildeComponent

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.Presentation

import qualified Wilde.ApplicationConstruction.Presentation.ButtonSequenceValue as BtnSeq



-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


objectListTableAccordingToSetup
  :: forall acc otConf atConf dbTable otNative idAtExisting idAtCreate.
     ATTRIBUTE_PRESENTATION atConf
  => WildeStyle
  -> AttributeTypeListSetup.Setup     otConf atConf dbTable otNative idAtExisting idAtCreate
  -> Maybe (FooterRowsConstructor acc otConf atConf dbTable otNative idAtExisting idAtCreate)
  -> [Presentation.Monad (idAtExisting -> AnySVALUE)]
  -> [Presentation.Monad (idAtExisting -> AnySVALUE)]
  -> Maybe StyledTitle
  -> [Object                          otConf atConf dbTable otNative idAtExisting idAtCreate]
  -> Presentation.Monad AnyCOMPONENT
objectListTableAccordingToSetup tableStyle
  atListSetup
  mbFooterRowsConstructor
  getLeftSideButtonConstructors
  getRightSideButtonConstructors
  mbTitle
  os =
    do
      leftSideContentConstructor  <- getMkSideContent getLeftSideButtonConstructors
      rightSideContentConstructor <- getMkSideContent getRightSideButtonConstructors
      let mkGenericTable = objectListTable
                           atListSetup mbFooterRowsConstructor
                           leftSideContentConstructor
                           rightSideContentConstructor
      genericTable      <- mkGenericTable mbTitle os
      let styledTable    = addStyleToSTYLING tableStyle genericTable
      pure $ AnyCOMPONENT $ TableListComponent styledTable
  where
    getMkSideContent :: [Presentation.Monad (idAtExisting -> AnySVALUE)]
                     -> Presentation.Monad (idAtExisting -> AnySVALUE)
    getMkSideContent getMkButtons  = do
      mkButtons <- sequence getMkButtons
      pure $ mkSideContent mkButtons

    mkSideContent :: [idAtExisting -> AnySVALUE] -> idAtExisting -> AnySVALUE
    mkSideContent mkButtons pk = BtnSeq.new $ map (\mkButton -> mkButton pk) mkButtons
