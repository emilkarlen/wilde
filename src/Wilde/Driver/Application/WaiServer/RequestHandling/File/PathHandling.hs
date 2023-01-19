{-# LANGUAGE OverloadedStrings #-}
module Wilde.Driver.Application.WaiServer.RequestHandling.File.PathHandling
(
    module Wilde.Driver.Application.WaiServer.RequestHandling.File.Types,
    FsPathPrefixAndRqPathSuffix,
    resolveMimeTypeFromValidPath,
    pathIsValid,
    filePathSepa,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.Either as Either
import           Control.Monad
import           Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import           Wilde.Driver.Application.Types
import           Wilde.Driver.Application.WaiServer.RequestHandling.File.Types


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


filePathSepa :: IsString a => a
filePathSepa = "/"

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
pathIsValid [] = True
pathIsValid components = noCompIsEmpty && noCompIsRelativeDir && lastCompHasAnExtension
    where
        noCompIsEmpty = mempty `notElem` components

        lastCompHasAnExtension = Either.isRight $ extensionOf components

        noCompIsRelativeDir = "."  `notElem` components &&
                              ".." `notElem` components


type FsPathPrefixAndRqPathSuffix = (FilePath, RequestPath)

-- | Resolves the mime type of a path that is valid according to `pathIsValid`.
-- Fails if the extension is not mapped to any mime type.
resolveMimeTypeFromValidPath
  :: MimeTypeMapping
  -> FsPathPrefixAndRqPathSuffix
     -- ^ (FS path prefix,
     --    RQ path suffix, that is appended to the
     --    FS path prefix to get the final FS path)
     --
     -- If the RQ path suffix is empty,
     -- then the FS path to serve is the same as the FS path prefix,
    --  and the mime type is derived from the FS path prefix
  -> Either ErrorMessage MimeType
resolveMimeTypeFromValidPath mimeTypes fsPAndrqS@(fsPathPrefix, rqPathSuffix) =
  do
    extension <- extensionOf $ pathToDeriveExtFrom fsPAndrqS
    case Map.lookup extension mimeTypes of
      Nothing -> Left $ "Extension not mapped: " <> extension
      Just mt -> Right mt

  where
    pathToDeriveFrom :: RequestPath
    pathToDeriveFrom
      | null rqPathSuffix = T.splitOn "/" $ T.pack fsPathPrefix
      | otherwise         = rqPathSuffix

extensionOf :: RequestPath -- ^ non-empty
            -> Either ErrorMessage T.Text
extensionOf requestPathComponents = do
  unless (length lastCompSplitOnExtSepa > 1)
    (extError "Path has no extension")
  let ext = last lastCompSplitOnExtSepa
  if ext == mempty
    then extError "Empty extension"
    else pure ext
  where
    lastCompSplitOnExtSepa :: [T.Text]
    lastCompSplitOnExtSepa = T.splitOn "." lastCompWoLeadingDots

    lastCompWoLeadingDots = T.dropWhile (=='.') (last requestPathComponents)

    extError :: T.Text -> Either ErrorMessage a
    extError cause = Left $ cause <> ": " <> T.intercalate filePathSepa requestPathComponents

pathToDeriveExtFrom :: FsPathPrefixAndRqPathSuffix -> RequestPath
pathToDeriveExtFrom (fsPathPrefix, []) = T.splitOn "/" $ T.pack fsPathPrefix
pathToDeriveExtFrom (_, rqPathSuffix)   = rqPathSuffix
