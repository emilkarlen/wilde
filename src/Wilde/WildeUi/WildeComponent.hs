-- | Some components used by Wilde.
module Wilde.WildeUi.WildeComponent
       (
         -- * Table

         TableListComponent(..),

         -- * Forms

         FormComponent(..),
         FormMethod(..),

         formButton,
         FormButtonType(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Render.Html.Types
import qualified Wilde.Render.Html.Element as HE
import qualified Wilde.Render.Html.Attribute as HA

import           Wilde.Render.AbstractTableToHtml

import           Wilde.GenericUi.Value (VALUE(..))

import           Wilde.WildeUi.WildeValue (AnySVALUE(..), withNeutralStyleAny)
import           Wilde.WildeUi.UiPrimitives
import           Wilde.WildeUi.WildeTable


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - TableListComponent -
-------------------------------------------------------------------------------


-- | A component containing a table list.
newtype TableListComponent = TableListComponent WildeTable

instance COMPONENT TableListComponent where
    componentHtml (TableListComponent wt) = Wilde.Render.AbstractTableToHtml.renderTable wt


-------------------------------------------------------------------------------
-- - FormComponent -
-------------------------------------------------------------------------------


data FormComponent =
  FormComponent
  {
    formAction  :: Maybe URL,
    formMethod  :: FormMethod,
    formContent :: [AnyCOMPONENT]
   }

data FormMethod = Get | Post

instance Show FormMethod where
  show Get  = "get"
  show Post = "post"

instance COMPONENT FormComponent where
    componentHtml (FormComponent action method content) =
      let
        contentHtml    = HE.seq $ map componentHtml content
        attrListAction = maybe [] (\actn -> [HA.action actn]) action
        formAttrs      = HA.method (show method) : attrListAction
      in
       HE.form contentHtml `HE.withAttrs` formAttrs


-------------------------------------------------------------------------------
-- - FormButtons -
-------------------------------------------------------------------------------


formButton :: FormButtonType -> String -> AnySVALUE
formButton buttonType label = AnySVALUE $ withNeutralStyleAny $ FormButton buttonType label

data FormButtonType
  = Submit
  | Reset

-- | A form button - type and label
data FormButton = FormButton FormButtonType String

instance VALUE FormButton where
  valueHtml (FormButton Submit label) = HE.submit label
  valueHtml (FormButton Reset  label) = HE.reset  label
