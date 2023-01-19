{-# LANGUAGE OverloadedStrings #-}
module WildeTest.Driver.WaiServer.FilePathHandling.ResolveMimeType
       (
         theTest,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import           Data.String (IsString(fromString))

import           WildeTest.Driver.WaiServer.FilePathHandling.TestResources

import qualified Wilde.Driver.Application.WaiServer.RequestHandling.File.PathHandling as Sut

import qualified TestResources.Testing.AssertUtils as Asrt

import           Test.HUnit


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


theTest :: Test
theTest =
  TestList
  [
    "WHEN RQ path suffix is non-empty THEN the mime type SHOULD be derived from the RQ path suffix" ~:
    TestList
    [
       "mime type is mapped"     ~: nonEmptyRequestPathSuffix_mapped
    ,  "mime type is not mapped" ~: nonEmptyRequestPathSuffix_notMapped
    ]

  , "WHEN RQ path suffix is empty THEN the mime type SHOULD be derived from the FS path prefix" ~:
    emptyRequestPathSuffix
  ]

nonEmptyRequestPathSuffix_mapped :: Test
nonEmptyRequestPathSuffix_mapped =
  TestList
  [
    "1 extension" ~:
    casesWithAndWithoutInitialDotsOfHead
    (compWExt "head" ext1)
    mimeType1

  , "WHEN 2 extension separators THEN last ext should be used" ~:
    casesWithAndWithoutInitialDotsOfHead
    (compWExt (compWExt "the-head" ext1) ext2) $
    mimeType2
  ]
  where
    casesWithAndWithoutInitialDotsOfHead :: T.Text -> Sut.MimeType -> Test
    casesWithAndWithoutInitialDotsOfHead lastPathCompWoInitDot expectedMimeType =
      TestList
      [
        "head wo dot-prefix" ~:
        tcs_wLastComponents
        [lastPathCompWoInitDot] $
        isMappedTo expectedMimeType

      , "head begins w 1 dot" ~:
        tcs_wLastComponents
        ["." <> lastPathCompWoInitDot] $
        isMappedTo expectedMimeType

      , "head begins w 2 dots" ~:
        tcs_wLastComponents
        [".." <> lastPathCompWoInitDot] $
        isMappedTo expectedMimeType

      ]
    ext1, ext2 :: T.Text
    ext1 = "ext1"
    ext2 = "ext2"

    fsPrefixWithNoMappedExt :: String
    fsPrefixWithNoMappedExt = "fs/prefix/w/not/mapped/ext"

    mimeType1, mimeType2 :: Sut.MimeType
    mimeType1 = "the mime type 1"
    mimeType2 = "the mime type 2"

    mtm :: Sut.MimeTypeMapping
    mtm = M.fromList [(ext1, mimeType1), (ext2, mimeType2)]

    isMappedTo :: Sut.MimeType -> Sut.RequestPath -> Assertion
    isMappedTo expectedMimeType rqpSuffix = asrtMapped mtm expectedMimeType (fsPrefixWithNoMappedExt, rqpSuffix)

nonEmptyRequestPathSuffix_notMapped :: Test
nonEmptyRequestPathSuffix_notMapped =
  TestList
  [
    "mapped extension appears in non-last path component" ~:
    tcs_wLastComponents
    [compWExt "a" ext_mapped, compWExt "b" ext_notMapped]
    isNotMapped

  , "mapped extension appears directly after initial dot" ~:
    tcs_wLastComponents
    [compWExt ("." <> ext_mapped) ext_notMapped]
    isNotMapped

  , "mapped extension appears directly after non-initial dot" ~:
    tcs_wLastComponents
    [compWExt (compWExt "head" ext_mapped) ext_notMapped]
    isNotMapped

  , "simple head.ext" ~:
    TestList
    [
      "empty mapping" ~:
      tcs_wLastComponents
      [compWExt "head" ext_notMapped]
      isNotMapped_emptyMapping

    , "non-empty mapping" ~:
      tcs_wLastComponents
      [compWExt "head" ext_notMapped]
      isNotMapped
    ]
  ]
  where
    ext_mapped, ext_notMapped :: T.Text
    ext_mapped    = "ext-mapped"
    ext_notMapped = "ext-not-mapped"

    fsPrefixWithNoMappedExt :: String
    fsPrefixWithNoMappedExt = "fs/prefix/w/not/mapped/ext"

    mimeType_mapped :: Sut.MimeType
    mimeType_mapped = "the mime type"

    mtm_wMappedExt :: Sut.MimeTypeMapping
    mtm_wMappedExt = M.singleton ext_mapped mimeType_mapped

    isNotMapped :: Sut.RequestPath -> Assertion
    isNotMapped rqpSuffix = asrtNotMapped mtm_wMappedExt (fsPrefixWithNoMappedExt, rqpSuffix)

    isNotMapped_emptyMapping :: Sut.RequestPath -> Assertion
    isNotMapped_emptyMapping rqpSuffix = asrtNotMapped mtm_wMappedExt (fsPrefixWithNoMappedExt, rqpSuffix)

emptyRequestPathSuffix :: Test
emptyRequestPathSuffix =
  TestList
  [
    "mapped"     ~: emptyRequestPathSuffix_mapped
   ,"not mapped" ~: emptyRequestPathSuffix_notMapped
  ]

emptyRequestPathSuffix_mapped :: Test
emptyRequestPathSuffix_mapped =
  TestList
  [
    "1 extension" ~:
    casesWithAndWithoutInitialDotsOfHead
    (compWExt_fs "head" ext1)
    mimeType1

  , "WHEN 2 extension separators THEN last ext should be used" ~:
    casesWithAndWithoutInitialDotsOfHead
    (compWExt_fs (compWExt_fs "the-head" ext1) ext2) $
    mimeType2
  ]
  where
    casesWithAndWithoutInitialDotsOfHead :: FilePath -> Sut.MimeType -> Test
    casesWithAndWithoutInitialDotsOfHead lastPathCompWoInitDot expectedMimeType =
      TestList
      [
        "head wo dot-prefix" ~:
        tcs_wLastComponents_fs
        lastPathCompWoInitDot $
        isMappedTo expectedMimeType

      , "head begins w 1 dot" ~:
        tcs_wLastComponents_fs
        ("." <> lastPathCompWoInitDot)$
        isMappedTo expectedMimeType

      , "head begins w 2 dots" ~:
        tcs_wLastComponents_fs
        (".." <> lastPathCompWoInitDot) $
        isMappedTo expectedMimeType

      ]
    ext1, ext2 :: String
    ext1 = "ext1"
    ext2 = "ext2"

    mimeType1, mimeType2 :: Sut.MimeType
    mimeType1 = "the mime type 1"
    mimeType2 = "the mime type 2"

    mtm :: Sut.MimeTypeMapping
    mtm = M.fromList [(fromString ext1, mimeType1), (fromString ext2, mimeType2)]

    isMappedTo :: Sut.MimeType -> FilePath -> Assertion
    isMappedTo expectedMimeType fspPrefix = asrtMapped mtm expectedMimeType (fspPrefix, [])

emptyRequestPathSuffix_notMapped :: Test
emptyRequestPathSuffix_notMapped =
  TestList
  [
    "mapped extension appears in non-last path component" ~:
    tcs_wLastComponents_fs
    (compWExt_fs "a" ext_mapped </> compWExt_fs "b" ext_notMapped)
    isNotMapped

  , "mapped extension appears directly after initial dot" ~:
    tcs_wLastComponents_fs
    (compWExt_fs ("." <> ext_mapped) ext_notMapped)
    isNotMapped

  , "mapped extension appears directly after non-initial dot" ~:
    tcs_wLastComponents_fs
    (compWExt_fs (compWExt_fs "head" ext_mapped) ext_notMapped)
    isNotMapped

  , "simple head.ext" ~:
    TestList
    [
      "empty mapping" ~:
      tcs_wLastComponents_fs
      (compWExt_fs "head" ext_notMapped)
      isNotMapped_emptyMapping

    , "non-empty mapping" ~:
      tcs_wLastComponents_fs
      (compWExt_fs "head" ext_notMapped)
      isNotMapped
    ]
  ]
  where
    ext_mapped, ext_notMapped :: String
    ext_mapped    = "ext-mapped"
    ext_notMapped = "ext-not-mapped"

    mimeType_mapped :: Sut.MimeType
    mimeType_mapped = "the mime type"

    mtm_wMappedExt :: Sut.MimeTypeMapping
    mtm_wMappedExt = M.singleton (fromString ext_mapped) mimeType_mapped

    isNotMapped :: FilePath -> Assertion
    isNotMapped fspPrefix = asrtNotMapped mtm_wMappedExt (fspPrefix, [])

    isNotMapped_emptyMapping :: FilePath -> Assertion
    isNotMapped_emptyMapping fspPrefix = asrtNotMapped mtm_wMappedExt (fspPrefix, [])

asrtMapped :: Sut.MimeTypeMapping -> Sut.MimeType
           -> Sut.FsPathPrefixAndRqPathSuffix -> Assertion
asrtMapped mtm expected actual =
  assertEqual "" (Right expected) $
  Sut.resolveMimeTypeFromValidPath mtm actual

asrtNotMapped :: Sut.MimeTypeMapping -> Sut.FsPathPrefixAndRqPathSuffix -> Assertion
asrtNotMapped mtm path = Asrt.isLeft $ Sut.resolveMimeTypeFromValidPath mtm path
