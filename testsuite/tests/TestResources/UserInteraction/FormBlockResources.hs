module TestResources.UserInteraction.FormBlockResources
       (
         -- * Checker functions

         Checker,
         WidgetChecker,
         PresentationOutputChecker,

         -- * Check of FormBlockRow

         checkFormBlockRowInfo,

         noCheck,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Test.HUnit

import qualified Text.Blaze.Html.Renderer.String as HRS

import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.WildeMedia as WildeMedia

import qualified TestResources.Testing.AssertUtils as AssertUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | A function that checks an expected value against the actual value, of
-- the same type.
type Checker a = String
                 -- ^ Error message header
                 -> a
                 -- ^ Expected value
                 -> a
                 -- ^ Actual value
                 -> Assertion

type WidgetChecker = Checker UiO.AnyWIDGET

type PresentationOutputChecker = Checker WildeMedia.PresentationOutput

-- | A checker that never fails.
noCheck :: Checker a
noCheck _ _ _ = pure ()

checkFormBlockRowInfo :: WidgetChecker
                      -> PresentationOutputChecker
                      -> String
                      -> UiO.FormBlockRowInfo
                      -> UiO.FormBlockRowInfo
                      -> Assertion
checkFormBlockRowInfo checkWidgets checkPresOutput msgHeader
  (UiO.FormBlockRowInfo {
      UiO.atFormBlockMetaValues = metas_expected,
      UiO.atFormBlockRow        = mbRow_expected
      })
  (UiO.FormBlockRowInfo {
      UiO.atFormBlockMetaValues = metas_actual,
      UiO.atFormBlockRow        = mbRow_actual
      })
  =
  do
    assertEqual "meta values" metas_expected metas_actual
    AssertUtils.checkMaybes
      (checkFormBlockRows checkWidgets checkPresOutput)
      showFormBlockRow
      (msgHeader ++ ": FormBlockRow")
      mbRow_expected
      mbRow_actual

showFormBlockRow :: UiO.FormBlockRow -> String
showFormBlockRow = either showLabelAndWidget showTitleAndPresentationOutput
  where
    showLabelAndWidget :: UiO.LabelAndWidget -> String
    showLabelAndWidget (label,UiO.AnyWIDGET widget) =
      "LabelAndWidget/label=" ++ show (UiO.labelString label) ++
      ":widget=" ++ widgetString
      where
        widgetString = HRS.renderHtml $ UiO.widgetHtml widget
    showTitleAndPresentationOutput :: (WildeMedia.Title,WildeMedia.PresentationOutput) -> String
    showTitleAndPresentationOutput (title,presentationOutput) =
      "TitleAndPresentationOutput/title=" ++ show title

checkFormBlockRows :: WidgetChecker
                   -> PresentationOutputChecker
                   -> String
                   -> UiO.FormBlockRow
                   -> UiO.FormBlockRow
                   -> Assertion
checkFormBlockRows widgetChecker presOutputChecker msgHeader
  expected
  actual
  =
    AssertUtils.checkEithers
    (checkLabelAndWidget widgetChecker)
    (checkTitleAndPresOutput presOutputChecker)
    (const "not impl: show function for LabelAndWidget")
    (const "not impl: show function for (Title,PresentationOutput)")
    msgHeader
    expected
    actual

checkTitleAndPresOutput :: PresentationOutputChecker
                        -> String
                        -> (WildeMedia.Title,WildeMedia.PresentationOutput)
                        -> (WildeMedia.Title,WildeMedia.PresentationOutput)
                        -> Assertion
checkTitleAndPresOutput checker msgHeader
  (title_expected,presOutput_expected)
  (title_actual, presOutput_actual) =
    do
      assertEqual msgTitle   title_expected      title_actual
      checker     msgPresOut presOutput_expected presOutput_actual
  where
    msgTitle   = msgHeader ++ ": title"
    msgPresOut = msgHeader ++ ": PresentationOutput"


checkLabelAndWidget :: WidgetChecker
                    -> String
                    -> UiO.LabelAndWidget
                    -- ^ Expected
                    -> UiO.LabelAndWidget
                    -- ^ Actual
                    -> Assertion
checkLabelAndWidget widgetChecker msgHeader
  (label_expected,widget_expected)
  (label_actual,widget_actual)
  =
    do
      assertEqual   msgLabels  label_expected  label_actual
      widgetChecker msgWidgets widget_expected widget_actual
  where
    msgLabels  = msgHeader ++ ": labels"
    msgWidgets = msgHeader ++ ": widgets"
