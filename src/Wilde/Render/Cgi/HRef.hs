module Wilde.Render.Cgi.HRef
(
    HRef(..),

    ServerVariables,
    ServerVariableElement,
)
where

import Wilde.Render.Html.Types (URL)
import Wilde.Render.Cgi.ServerVariables

-- | A HTML href
data HRef = HRef
  {
    hrefUnencodedUrlBase :: URL
  , hrefUnencodedUrlArgs :: ServerVariables
  }
