module Wilde.Render.Cgi.HRef 
(
    HRef(..),

    ServerVariables,
    ServerVariableElement,
)
where

import Wilde.Render.Cgi.ServerVariables

-- | A HTML href
data HRef = HRef
  {
    hrefUnencodedUrlBase :: String
  , hrefUnencodedUrlArgs :: ServerVariables
  }
