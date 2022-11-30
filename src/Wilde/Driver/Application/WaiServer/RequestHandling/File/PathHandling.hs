{-# LANGUAGE OverloadedStrings #-}
module Wilde.Driver.Application.WaiServer.RequestHandling.File.PathHandling
(
    module Wilde.Driver.Application.WaiServer.RequestHandling.File.Types,
    resolveMimeTypeFromValidPath,
    pathIsValid,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Either as Either
import           Control.Monad
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import           Wilde.Driver.Application.Types
import           Wilde.Driver.Application.WaiServer.RequestHandling.File.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


{- | A path is valid iff

 - it is non empty (directories are not served), and
 - it contains no empty components (the last component must not be empty,
   since it would represent a dir; but empty non-last compoenents could be
   allowed to be empty, but lets say that is invalid...), and
 - no component is one of the relative dirs (".", ".."), and
 - the last component has an extension
   (i.e. contains at least one dot, and that dot must appear after
   a non-dot character so that the ext of ".hidden.txt" is "txt",
   also, this extension is non-empty)
-}
pathIsValid :: RequestPath -> Bool
pathIsValid [] = False
pathIsValid components = noCompIsEmpty && noCompIsRelativeDir && lastCompHasAnExtension
    where
        noCompIsEmpty = mempty `notElem` components

        lastCompHasAnExtension = Either.isRight $ extensionOf components

        noCompIsRelativeDir = "."  `notElem` components &&
                              ".." `notElem` components


-- | Resolves the mime type of a path that is valid according to `pathIsValid`.
-- Fails if the extension is not mapped to any mime type.
resolveMimeTypeFromValidPath :: MimeTypeMapping -> RequestPath -> Either ErrorMessage MimeType
resolveMimeTypeFromValidPath mimeTypes requestPath =
  do
    extension <- extensionOf requestPath
    -- case mimeTypes Map.! extension of
    case Map.lookup extension mimeTypes of
      Nothing -> Left "error message" -- TODO "Not handled"
      Just mt -> Right mt

extensionOf :: RequestPath -- ^ non-empty
            -> Either ErrorMessage T.Text
extensionOf requestPathComponents = do
  unless (length lastCompSplitOnExtSepa > 1)
    (Left "error message") -- TODO "Missing file extension"
  let ext = last lastCompSplitOnExtSepa
  if ext == mempty
    then Left "error message" -- TODO "Missing file extension"
    else pure ext
  where
    lastCompSplitOnExtSepa :: [T.Text]
    lastCompSplitOnExtSepa = T.splitOn "." lastCompWoLeadingDots
    
    lastCompWoLeadingDots = T.dropWhile (=='.') (last requestPathComponents)

