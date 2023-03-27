-- | Functionality for rendering User Interaction in terms of COMPONENT.
--
-- The idea is that 'ObjectModelUserInteraction' should not need
-- knowledge of output medias - it should be "rendition independent".

module Wilde.Render.UserInteractionRendering
       (
         metaValueInput,
         cgiValueInput,
         )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Render.Html.Types hiding (Label)
import qualified Wilde.Render.Html.Element as HE
import qualified Wilde.Render.Html.Attribute as HA


import           Wilde.Media.UserInteraction as UI

import           Wilde.WildeUi.LayoutComponents as LayoutComponents


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Label -
-------------------------------------------------------------------------------


instance VALUE Label where
    valueHtml label = HE.str $ labelString label


-------------------------------------------------------------------------------
-- - Widget -
-------------------------------------------------------------------------------


-- | Constructs a Html "input" tag with type and key.
inputWithTypeAndKey :: String -- ^ input type (text, checkbox, ...)
                    -> ElementKey
                    -> Html
inputWithTypeAndKey inputType key = withKey key $ HE.input `HE.withAttrs` [HA.type_ inputType]

-- | Adds a key to the given Html.
withKey :: ElementKey -> Html -> Html
withKey key html = html `HE.withAttrs` [HA.name (elementKeyRender key)]


-------------------------------------------------------------------------------
-- - utilities -
-------------------------------------------------------------------------------

-- | A HTML "input" element for a Meta Element.
metaValueInput :: Element -> Html
metaValueInput (elementKey,value) =
  HE.input `HE.withAttrs` [HA.type_hidden,
                           HA.name (elementKeyRender elementKey),
                           HA.value value]

-- | A HTML "input" element for a CGI value.
cgiValueInput :: (String,Maybe String) -> Html
cgiValueInput (key,Just value) = HE.input `HE.withAttrs` [HA.type_hidden,
                                                          HA.name key,
                                                          HA.value value]
cgiValueInput (key,Nothing) = HE.input `HE.withAttrs` [HA.type_hidden,
                                                       HA.name key]
