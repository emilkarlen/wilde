-- | Some components used by Wilde.
module Wilde.WildeUi.WildeComponent
       (
         -- * Table

         WildeTableDatatype,
         TableListComponent(..),

         -- * Forms

         FormComponent(..),
         FormMethod(..),
         FormButtons(..),

       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Text.Html  as H hiding (HtmlTable)

import Wilde.Render.AbstractTableToHtml

import Wilde.Media.WildeMedia

import Wilde.Render.RenderAsHtml


-------------------------------------------------------------------------------
-- - WildeTableDatatype -
-------------------------------------------------------------------------------


newtype WildeTableDatatype = WildeTableDatatype WildeTable

instance COMPONENT WildeTableDatatype where
    componentHtml (WildeTableDatatype x) = Wilde.Render.AbstractTableToHtml.renderTable x


-------------------------------------------------------------------------------
-- - TableListComponent -
-------------------------------------------------------------------------------


-- | A component containing a table list.
data TableListComponent = TableListComponent
    {
      tlcTitle :: Maybe StyledTitle,
      tlcTable :: WildeTable
    }

instance COMPONENT TableListComponent where
    componentHtml tlc = renderComponent (tlcTitle tlc) $
                        Wilde.Render.AbstractTableToHtml.renderTable (tlcTable tlc)

-------------------------------------------------------------------------------
-- - FormComponent -
-------------------------------------------------------------------------------


data FormComponent =
  FormComponent
  {
    formAction  :: Maybe String,
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
        contentHtml    = H.concatHtml $ map componentHtml content
        attrListAction = maybe [] (\actn -> [H.action actn]) action
        formAttrs      = H.method (show method) : attrListAction
      in
       H.form contentHtml H.! formAttrs


-------------------------------------------------------------------------------
-- - FormButtons -
-------------------------------------------------------------------------------


data FormButtons =
  FormButtons
  {
    formButtonsSubmitText :: String
  , formButtonsResetText  :: String
  }

instance COMPONENT FormButtons where
  componentHtml (FormButtons submitText resetText) =
    let
      attrsSubmit = [H.thetype "submit",H.value submitText]
    in
     (H.reset "reset-button" resetText) H.+++ (H.input H.! attrsSubmit)
